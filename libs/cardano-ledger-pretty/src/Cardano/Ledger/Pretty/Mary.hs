{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Pretty.Mary where

import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary.TxBody
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Pretty
import qualified Data.Map as Map
import Data.Text (Text)
import Lens.Micro
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
    [ ("invalidBefore", ppStrictMaybe ppSlotNo b)
    , ("invalidHereafter", ppStrictMaybe ppSlotNo a)
    ]

instance PrettyA ValidityInterval where prettyA = ppValidityInterval

ppAuxiliaryData :: Era era => AllegraTxAuxData era -> PDoc
ppAuxiliaryData (AllegraTxAuxData m sp) =
  ppRecord
    "AllegraTxAuxData"
    [ ("metadata", ppMap' (text "Metadata") ppWord64 ppMetadatum m)
    , ("auxiliaryScripts", ppStrictSeq prettyA sp)
    ]

instance Era era => PrettyA (AllegraTxAuxData era) where
  prettyA = ppAuxiliaryData

allegraFields ::
  ( AllegraEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , ProtVerAtMost era 8
  , PrettyA (DCert era)
  ) =>
  TxBody era ->
  [(Text, PDoc)]
allegraFields txBody =
  [ ("inputs", ppSet ppTxIn (txBody ^. inputsTxBodyL))
  , ("outputs", ppStrictSeq prettyA (txBody ^. outputsTxBodyL))
  , ("certificates", ppStrictSeq prettyA (txBody ^. certsTxBodyL))
  , ("withdrawals", ppWithdrawals (txBody ^. withdrawalsTxBodyL))
  , ("txfee", ppCoin (txBody ^. feeTxBodyL))
  , ("vldt", ppValidityInterval (txBody ^. vldtTxBodyL))
  , ("update", ppStrictMaybe ppUpdate (txBody ^. updateTxBodyL))
  , ("auxDataHash", ppStrictMaybe ppAuxiliaryDataHash (txBody ^. auxDataHashTxBodyL))
  ]

instance
  ( AllegraEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , TxBody era ~ AllegraTxBody era
  , ProtVerAtMost era 8
  , PrettyA (DCert era)
  ) =>
  PrettyA (AllegraTxBody era)
  where
  prettyA txBody = ppRecord "AllegraTxBody" (allegraFields txBody)

instance
  ( MaryEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , TxBody era ~ MaryTxBody era
  , ProtVerAtMost era 8
  , PrettyA (DCert era)
  ) =>
  PrettyA (MaryTxBody era)
  where
  prettyA txBody =
    ppRecord
      "MaryTxBody"
      ( allegraFields txBody
          ++ [("mint", prettyA (txBody ^. mintTxBodyL))]
      )
