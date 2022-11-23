{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Pretty.Mary where

import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Pretty hiding (ppTxBody)
import Cardano.Ledger.Shelley.TxBody (ShelleyTxOut)
import Cardano.Ledger.ShelleyMA.AuxiliaryData
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.ShelleyMA.TxBody
import qualified Data.Map as Map
import Prettyprinter (hsep, viaShow)

ppPolicyID :: PolicyID c -> PDoc
ppPolicyID (PolicyID sh) = ppScriptHash sh

instance PrettyA (PolicyID c) where prettyA x = ppSexp "PolicyID" [ppPolicyID x]

ppAssetName :: AssetName -> PDoc
ppAssetName = viaShow

instance PrettyA AssetName where prettyA x = ppSexp "AssetName" [ppAssetName x]

ppMultiAsset :: MultiAsset c -> PDoc
ppMultiAsset m = ppList pptriple (flattenMultiAsset m)
  where
    pptriple (i, asset, num) = hsep [ppPolicyID i, ppAssetName asset, ppInteger num]

instance CC.Crypto c => PrettyA (MultiAsset c) where
  prettyA x = ppSexp "MultiAsset" [ppMultiAsset x]

ppValue :: MaryValue c -> PDoc
ppValue (MaryValue n m) = ppSexp "Value" $ [ppCoin (Coin n), ppMultiAsset m] ++ ppBad
  where
    ppBad = case getBadMultiAsset m of
      [] -> []
      bad -> [ppString "Bad " <> ppList ppPolicyID bad]
    getBadMultiAsset (MultiAsset ma) = Map.keys (Map.filter Map.null ma)

instance PrettyA (MaryValue c) where prettyA = ppValue

ppTimelock :: Era era => Timelock era -> PDoc
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

instance Era era => PrettyA (Timelock era) where prettyA = ppTimelock

ppValidityInterval :: ValidityInterval -> PDoc
ppValidityInterval (ValidityInterval b a) =
  ppRecord
    "ValidityInterval"
    [ ("invalidBefore", ppStrictMaybe ppSlotNo b),
      ("invalidHereafter", ppStrictMaybe ppSlotNo a)
    ]

instance PrettyA ValidityInterval where prettyA = ppValidityInterval

ppAuxiliaryData :: Era era => PrettyA (Core.Script era) => AllegraTxAuxData era -> PDoc
ppAuxiliaryData (AllegraTxAuxData' m sp) =
  ppRecord
    "AllegraTxAuxData"
    [ ("metadata", ppMap' (text "Metadata") ppWord64 ppMetadatum m),
      ("auxiliaryscripts", ppStrictSeq prettyA sp)
    ]

instance (Era era, PrettyA (Core.Script era)) => PrettyA (AllegraTxAuxData era) where
  prettyA = ppAuxiliaryData

ppTxBody ::
  ( Core.EraTxOut era,
    PrettyA (Core.Value era),
    PrettyA (Core.PParamsUpdate era),
    Core.TxOut era ~ ShelleyTxOut era
  ) =>
  MATxBody era ->
  PDoc
ppTxBody (MATxBody i o d w fee vi u m mnt) =
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
  ( Core.EraTxOut era,
    PrettyA (Core.Value era),
    PrettyA (Core.PParamsUpdate era),
    Core.TxOut era ~ ShelleyTxOut era
  ) =>
  PrettyA (MATxBody era)
  where
  prettyA = ppTxBody
