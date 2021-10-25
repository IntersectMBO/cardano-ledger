{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Pretty.Mary where

import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Pretty hiding (ppTxBody)
import Cardano.Ledger.ShelleyMA.AuxiliaryData
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.ShelleyMA.TxBody
import Prettyprinter (hsep)

ppValue :: Value crypto -> PDoc
ppValue v = case gettriples' v of
  (n, triples, []) -> ppSexp "Value" [ppCoin (Coin n), ppList pptriple triples]
  (n, triples, bad) -> ppSexp "Value" [ppCoin (Coin n), ppList pptriple triples, ppString "Bad " <> ppList ppPolicyID bad]
  where
    pptriple (i, asset, num) = hsep [ppPolicyID i, ppAssetName asset, ppInteger num]

ppPolicyID :: PolicyID crypto -> PDoc
ppPolicyID (PolicyID sh) = ppScriptHash sh

ppAssetName :: AssetName -> PDoc
ppAssetName (AssetName bs) = ppLong bs

instance PrettyA (Value crypto) where prettyA = ppValue

instance PrettyA (PolicyID crypto) where prettyA x = ppSexp "PolicyID" [ppPolicyID x]

instance PrettyA AssetName where prettyA x = ppSexp "AssetName" [ppAssetName x]

ppTimelock :: CC.Crypto crypto => Timelock crypto -> PDoc
ppTimelock (RequireSignature akh) =
  ppSexp "Signature" [ppKeyHash akh]
ppTimelock (RequireAllOf ms) =
  ppSexp "AllOf" (foldr (:) [] (fmap ppTimelock ms))
ppTimelock (RequireAnyOf ms) =
  ppSexp "AnyOf" (foldr (:) [] (fmap ppTimelock ms))
ppTimelock (RequireMOf m ms) =
  ppSexp "MOfN" (ppInteger (fromIntegral m) : foldr (:) [] (fmap ppTimelock ms))
ppTimelock (RequireTimeExpire mslot) =
  ppSexp "Expires" [ppSlotNo mslot]
ppTimelock (RequireTimeStart mslot) =
  ppSexp "Starts" [ppSlotNo mslot]

instance CC.Crypto crypto => PrettyA (Timelock crypto) where prettyA = ppTimelock

ppValidityInterval :: ValidityInterval -> PDoc
ppValidityInterval (ValidityInterval b a) =
  ppRecord
    "ValidityInterval"
    [ ("invalidBefore", ppStrictMaybe ppSlotNo b),
      ("invalidHereafter", ppStrictMaybe ppSlotNo a)
    ]

instance PrettyA ValidityInterval where prettyA = ppValidityInterval

ppAuxiliaryData :: PrettyA (Core.Script era) => AuxiliaryData era -> PDoc
ppAuxiliaryData (AuxiliaryData' m sp) =
  ppRecord
    "AuxiliaryData"
    [ ("metadata", ppMap' (text "Metadata") ppWord64 ppMetadatum m),
      ("auxiliaryscripts", ppStrictSeq prettyA sp)
    ]

instance PrettyA (Core.Script era) => PrettyA (AuxiliaryData era) where
  prettyA = ppAuxiliaryData

ppTxBody ::
  ( Era era,
    PrettyA (Core.Value era),
    PrettyA (Core.PParamsDelta era)
  ) =>
  TxBody era ->
  PDoc
ppTxBody (TxBody' i o d w fee vi u m mnt) =
  ppRecord
    "TxBody(Mary or Allegra)"
    [ ("inputs", ppSet ppTxIn i),
      ("outputs", ppStrictSeq ppTxOut o),
      ("certificates", ppStrictSeq ppDCert d),
      ("withdrawals", ppWdrl w),
      ("txfee", ppCoin fee),
      ("vldt", ppValidityInterval vi),
      ("update", ppStrictMaybe ppUpdate u),
      ("auxDataHash", ppStrictMaybe ppAuxiliaryDataHash m),
      ("mint", prettyA mnt)
    ]

instance
  ( Era era,
    PrettyA (Core.Value era),
    PrettyA (Core.PParamsDelta era)
  ) =>
  PrettyA (TxBody era)
  where
  prettyA = ppTxBody
