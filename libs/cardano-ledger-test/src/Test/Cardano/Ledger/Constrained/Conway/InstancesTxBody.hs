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

module Test.Cardano.Ledger.Constrained.Conway.InstancesTxBody where

import Cardano.Ledger.Address (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Core (AllegraEraTxBody (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..), ValidityInterval (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), AlonzoTxBody (..), ScriptIntegrityHash)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Binary (EncCBOR (..))
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
import Test.Cardano.Ledger.Constrained.Conway.Instances

-- ==============================================================================
{-

-- TxBody HasSpec instance ------------------------------------------------

-- NOTE: this is a representation of the `ConwayTxBody` type. You can't
-- simply use the generics to derive the `SimpleRep` for `ConwayTxBody`
-- because the type is memoized. So instead we say that the representation
-- is the same as what you would get from using the `ConwayTxBody` pattern.
type ConwayTxBodyTypes c =
  '[ Set (TxIn (EraCrypto (ConwayEra c)))
   , Set (TxIn (EraCrypto (ConwayEra c)))
   , Set (TxIn (EraCrypto (ConwayEra c)))
   , StrictSeq (Sized (TxOut (ConwayEra c)))
   , StrictMaybe (Sized (TxOut (ConwayEra c)))
   , StrictMaybe Coin
   , SOS.OSet (ConwayTxCert (ConwayEra c))
   , Withdrawals (EraCrypto (ConwayEra c))
   , Coin
   , ValidityInterval
   , Set (KeyHash 'Witness (EraCrypto (ConwayEra c)))
   , MultiAsset (EraCrypto (ConwayEra c))
   , StrictMaybe (ScriptIntegrityHash (EraCrypto (ConwayEra c)))
   , StrictMaybe (AuxiliaryDataHash (EraCrypto (ConwayEra c)))
   , StrictMaybe Network
   , VotingProcedures (ConwayEra c)
   , SOS.OSet (ProposalProcedure (ConwayEra c))
   , StrictMaybe Coin
   , Coin
   ]
instance (EraPP (ConwayEra c), IsConwayUniv fn, Crypto c) => HasSpec fn (ConwayTxBody (ConwayEra c))

instance Crypto c => HasSimpleRep (ConwayTxBody (ConwayEra c)) where
  type SimpleRep (ConwayTxBody (ConwayEra c)) = SOP '["ConwayTxBody" ::: ConwayTxBodyTypes c]
  toSimpleRep ConwayTxBody {..} =
    inject @"ConwayTxBody" @'["ConwayTxBody" ::: ConwayTxBodyTypes c]
      ctbSpendInputs
      ctbCollateralInputs
      ctbReferenceInputs
      ctbOutputs
      ctbCollateralReturn
      ctbTotalCollateral
      ctbCerts
      ctbWithdrawals
      ctbTxfee
      ctbVldt
      ctbReqSignerHashes
      ctbMint
      ctbScriptIntegrityHash
      ctbAdHash
      ctbTxNetworkId
      ctbVotingProcedures
      ctbProposalProcedures
      ctbCurrentTreasuryValue
      ctbTreasuryDonation
  fromSimpleRep rep =
    algebra @'["ConwayTxBody" ::: ConwayTxBodyTypes c] rep ConwayTxBody
-}

instance HasSimpleRep (Update era)
instance (EraPP era, IsConwayUniv fn) => HasSpec fn (Update era)

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
  ( EraPP era
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
  ( EraPP era
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
  ( EraPP era
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

-- | This is an abstraction of the Pattern MaryTxBody, that uses [x] instead of (StrictSeq x)
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
  toSimpleRep (AlonzoTxBody colis is os certs w c vi up kh ma ihash aux nw) =
    inject @"AlonzoTxBody" @'["AlonzoTxBody" ::: AlonzoTxBodyTypes era]
      colis
      is
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
      ( \colis is os certs w c vi up kh ma ihash aux nw ->
          AlonzoTxBody
            colis
            is
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
  ( EraPP era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , HasSpec fn (TxCert era)
  ) =>
  HasSpec fn (AlonzoTxBody era)

fromAlonzoBody :: forall era. AlonzoEraTxBody era => AlonzoTxBody era -> TxBody era
fromAlonzoBody (AlonzoTxBody colinputs inputs outputs certs withdrawals coin vi _up kh ma ihash aux nw) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
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
