{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Ledger.Epoch where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Ledger.Epoch.GlobalConstants
d_GlobalConstants_4 = ()
data T_GlobalConstants_4
  = C_GlobalConstants'46'constructor_33 Integer
                                        MAlonzo.Code.Data.Nat.Base.T_NonZero_88 Integer Integer
                                        AgdaAny
-- Ledger.Epoch.GlobalConstants.Network
d_Network_18 :: T_GlobalConstants_4 -> ()
d_Network_18 = erased
-- Ledger.Epoch.GlobalConstants.SlotsPerEpochᶜ
d_SlotsPerEpoch'7580'_20 :: T_GlobalConstants_4 -> Integer
d_SlotsPerEpoch'7580'_20 v0
  = case coe v0 of
      C_GlobalConstants'46'constructor_33 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.GlobalConstants.NonZero-SlotsPerEpoch
d_NonZero'45'SlotsPerEpoch_22 ::
  T_GlobalConstants_4 -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_NonZero'45'SlotsPerEpoch_22 v0
  = case coe v0 of
      C_GlobalConstants'46'constructor_33 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.GlobalConstants.StabilityWindowᶜ
d_StabilityWindow'7580'_24 :: T_GlobalConstants_4 -> Integer
d_StabilityWindow'7580'_24 v0
  = case coe v0 of
      C_GlobalConstants'46'constructor_33 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.GlobalConstants.Quorum
d_Quorum_26 :: T_GlobalConstants_4 -> Integer
d_Quorum_26 v0
  = case coe v0 of
      C_GlobalConstants'46'constructor_33 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.GlobalConstants.NetworkId
d_NetworkId_28 :: T_GlobalConstants_4 -> AgdaAny
d_NetworkId_28 v0
  = case coe v0 of
      C_GlobalConstants'46'constructor_33 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure
d_EpochStructure_30 = ()
data T_EpochStructure_30
  = C_EpochStructure'46'constructor_543 MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
                                        (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                        MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
                                        AgdaAny (AgdaAny -> AgdaAny)
                                        MAlonzo.Code.Interface.DecEq.T_DecEq_14
-- Ledger.Epoch.EpochStructure.Slotʳ
d_Slot'691'_52 ::
  T_EpochStructure_30 -> MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_Slot'691'_52 v0
  = case coe v0 of
      C_EpochStructure'46'constructor_543 v1 v3 v4 v6 v7 v8 v9 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure.Epoch
d_Epoch_54 :: T_EpochStructure_30 -> ()
d_Epoch_54 = erased
-- Ledger.Epoch.EpochStructure.Slot
d_Slot_56 :: T_EpochStructure_30 -> ()
d_Slot_56 = erased
-- Ledger.Epoch.EpochStructure.epoch
d_epoch_58 :: T_EpochStructure_30 -> AgdaAny -> AgdaAny
d_epoch_58 v0
  = case coe v0 of
      C_EpochStructure'46'constructor_543 v1 v3 v4 v6 v7 v8 v9 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure.firstSlot
d_firstSlot_60 :: T_EpochStructure_30 -> AgdaAny -> AgdaAny
d_firstSlot_60 v0
  = case coe v0 of
      C_EpochStructure'46'constructor_543 v1 v3 v4 v6 v7 v8 v9 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure._<ˢ_
d__'60''738'__62 :: T_EpochStructure_30 -> AgdaAny -> AgdaAny -> ()
d__'60''738'__62 = erased
-- Ledger.Epoch.EpochStructure.Slot-STO
d_Slot'45'STO_64 ::
  T_EpochStructure_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_Slot'45'STO_64 v0
  = case coe v0 of
      C_EpochStructure'46'constructor_543 v1 v3 v4 v6 v7 v8 v9 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure.StabilityWindow
d_StabilityWindow_66 :: T_EpochStructure_30 -> AgdaAny
d_StabilityWindow_66 v0
  = case coe v0 of
      C_EpochStructure'46'constructor_543 v1 v3 v4 v6 v7 v8 v9 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure.sucᵉ
d_suc'7497'_68 :: T_EpochStructure_30 -> AgdaAny -> AgdaAny
d_suc'7497'_68 v0
  = case coe v0 of
      C_EpochStructure'46'constructor_543 v1 v3 v4 v6 v7 v8 v9 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure.DecEq-Epoch
d_DecEq'45'Epoch_70 ::
  T_EpochStructure_30 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Epoch_70 v0
  = case coe v0 of
      C_EpochStructure'46'constructor_543 v1 v3 v4 v6 v7 v8 v9 -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Epoch.EpochStructure._≥ˢ_
d__'8805''738'__72 ::
  T_EpochStructure_30 -> AgdaAny -> AgdaAny -> ()
d__'8805''738'__72 = erased
-- Ledger.Epoch.EpochStructure._≤ˢ_
d__'8804''738'__74 ::
  T_EpochStructure_30 -> AgdaAny -> AgdaAny -> ()
d__'8804''738'__74 = erased
-- Ledger.Epoch.EpochStructure._._<?_
d__'60''63'__78 ::
  T_EpochStructure_30 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__78 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du__'60''63'__516
      (coe d_Slot'45'STO_64 (coe v0))
-- Ledger.Epoch.EpochStructure._≤ˢ?_
d__'8804''738''63'__84 ::
  T_EpochStructure_30 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''738''63'__84 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du__'60''63'__516
         (d_Slot'45'STO_64 (coe v0)) v2 v1)
-- Ledger.Epoch.EpochStructure._+ᵉ_
d__'43''7497'__90 ::
  T_EpochStructure_30 -> Integer -> AgdaAny -> AgdaAny
d__'43''7497'__90 v0 v1 v2
  = case coe v1 of
      0 -> coe v2
      _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             d_suc'7497'_68 v0 (d__'43''7497'__90 (coe v0) (coe v3) (coe v2))
-- Ledger.Epoch.EpochStructure._+ᵉ'_
d__'43''7497'''__98 ::
  T_EpochStructure_30 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43''7497'''__98 v0 v1 v2
  = coe
      d_epoch_58 v0
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'43'__2010
         (d_Slot'691'_52 (coe v0)) (coe d_firstSlot_60 v0 v1)
         (coe d_firstSlot_60 v0 v2))
-- Ledger.Epoch.EpochStructure._<ᵉ_
d__'60''7497'__246 ::
  T_EpochStructure_30 -> AgdaAny -> AgdaAny -> ()
d__'60''7497'__246 = erased
-- Ledger.Epoch.EpochStructure._≤ᵉ_
d__'8804''7497'__252 ::
  T_EpochStructure_30 -> AgdaAny -> AgdaAny -> ()
d__'8804''7497'__252 = erased
-- Ledger.Epoch.EpochStructure._≤ᵉ?_
d__'8804''7497''63'__262 ::
  T_EpochStructure_30 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''7497''63'__262 v0 v1 v2
  = coe
      d__'8804''738''63'__84 (coe v0) (coe d_firstSlot_60 v0 v1)
      (coe d_firstSlot_60 v0 v2)
-- Ledger.Epoch._._.Network
d_Network_276 :: T_GlobalConstants_4 -> ()
d_Network_276 = erased
-- Ledger.Epoch._._.NetworkId
d_NetworkId_278 :: T_GlobalConstants_4 -> AgdaAny
d_NetworkId_278 v0 = coe d_NetworkId_28 (coe v0)
-- Ledger.Epoch._._.NonZero-SlotsPerEpoch
d_NonZero'45'SlotsPerEpoch_280 ::
  T_GlobalConstants_4 -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_NonZero'45'SlotsPerEpoch_280 v0
  = coe d_NonZero'45'SlotsPerEpoch_22 (coe v0)
-- Ledger.Epoch._._.Quorum
d_Quorum_282 :: T_GlobalConstants_4 -> Integer
d_Quorum_282 v0 = coe d_Quorum_26 (coe v0)
-- Ledger.Epoch._._.SlotsPerEpochᶜ
d_SlotsPerEpoch'7580'_284 :: T_GlobalConstants_4 -> Integer
d_SlotsPerEpoch'7580'_284 v0
  = coe d_SlotsPerEpoch'7580'_20 (coe v0)
-- Ledger.Epoch._._.StabilityWindowᶜ
d_StabilityWindow'7580'_286 :: T_GlobalConstants_4 -> Integer
d_StabilityWindow'7580'_286 v0
  = coe d_StabilityWindow'7580'_24 (coe v0)
-- Ledger.Epoch._.ℕEpochStructure
d_ℕEpochStructure_288 :: T_GlobalConstants_4 -> T_EpochStructure_30
d_ℕEpochStructure_288 v0
  = coe
      C_EpochStructure'46'constructor_543
      MAlonzo.Code.Data.Nat.Properties.d_'43''45''42''45'semiring_3648
      (\ v1 ->
         coe
           MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v1)
           (coe d_SlotsPerEpoch'7580'_20 (coe v0)))
      (\ v1 -> mulInt (coe v1) (coe d_SlotsPerEpoch'7580'_20 (coe v0)))
      MAlonzo.Code.Data.Nat.Properties.d_'60''45'isStrictTotalOrder_2878
      (d_StabilityWindow'7580'_24 (coe v0))
      (\ v1 -> addInt (coe (1 :: Integer)) (coe v1))
      MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
