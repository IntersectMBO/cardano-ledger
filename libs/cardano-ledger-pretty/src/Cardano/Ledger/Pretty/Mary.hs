{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Ledger.Pretty.Mary () where

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
import Prettyprinter (hsep)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Mary (MaryEra)

instance PrettyA (PolicyID c) where
  prettyA (PolicyID sh) = prettyA sh

instance PrettyA AssetName

ppMultiAsset :: MultiAsset c -> PDoc
ppMultiAsset m = mconcat $ pptriple <$> flattenMultiAsset m
  where
    pptriple (i, asset, num) = hsep [prettyA i, prettyA asset, prettyA num]

instance CC.Crypto c => PrettyA (MultiAsset c) where
  prettyA x = ppSexp "MultiAsset" [ppMultiAsset x]

ppValue :: MaryValue c -> PDoc
ppValue (MaryValue n m) = ppSexp "Value" $ [prettyA (Coin n), ppMultiAsset m] ++ ppBad
  where
    ppBad = case getBadMultiAsset m of
      [] -> []
      bad -> ["Bad " <> prettyA bad]
    getBadMultiAsset (MultiAsset ma) = Map.keys (Map.filter Map.null ma)

instance PrettyA (MaryValue c) where prettyA = ppValue

ppTimelock :: Era era => Timelock era -> PDoc
ppTimelock (RequireSignature akh) =
  ppSexp "Signature" [prettyA akh]
ppTimelock (RequireAllOf ms) =
  ppSexp "AllOf" (foldr (:) [] (fmap ppTimelock ms))
ppTimelock (RequireAnyOf ms) =
  ppSexp "AnyOf" (foldr (:) [] (fmap ppTimelock ms))
ppTimelock (RequireMOf m ms) =
  ppSexp "MOfN" (prettyA m : foldr (:) [] (fmap ppTimelock ms))
ppTimelock (RequireTimeExpire mslot) =
  ppSexp "Expires" [prettyA mslot]
ppTimelock (RequireTimeStart mslot) =
  ppSexp "Starts" [prettyA mslot]

instance Era era => PrettyA (Timelock era) where prettyA = ppTimelock

ppValidityInterval :: ValidityInterval -> PDoc
ppValidityInterval (ValidityInterval b a) =
  ppRecord
    "ValidityInterval"
    [ ("invalidBefore", prettyA b)
    , ("invalidHereafter", prettyA a)
    ]

instance PrettyA ValidityInterval where prettyA = ppValidityInterval

instance Era era => PrettyA (AllegraTxAuxData era) where
  prettyA (AllegraTxAuxData' m sp) =
    ppRecord
      "AllegraTxAuxData"
      [ ("metadata", prettyA m)
      , ("auxiliaryscripts", prettyA sp)
      ]

allegraFields ::
  ( AllegraEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , ProtVerAtMost era 8
  ) =>
  TxBody era ->
  [(Text, PDoc)]
allegraFields txBody =
  [ ("inputs", prettyA (txBody ^. inputsTxBodyL))
  , ("outputs", prettyA (txBody ^. outputsTxBodyL))
  , ("certificates", prettyA (txBody ^. certsTxBodyG))
  , ("withdrawals", prettyA (txBody ^. withdrawalsTxBodyL))
  , ("txfee", prettyA (txBody ^. feeTxBodyL))
  , ("vldt", prettyA (txBody ^. vldtTxBodyL))
  , ("update", prettyA (txBody ^. updateTxBodyL))
  , ("auxDataHash", prettyA (txBody ^. auxDataHashTxBodyL))
  ]

instance
  ( AllegraEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , TxBody era ~ AllegraTxBody era
  , ProtVerAtMost era 8
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
  ) =>
  PrettyA (MaryTxBody era)
  where
  prettyA txBody =
    ppRecord
      "MaryTxBody"
      ( allegraFields txBody
          ++ [("mint", prettyA (txBody ^. mintTxBodyL))]
      )

instance PrettyA (PParams (AllegraEra c)) where
  prettyA = undefined

instance PrettyA (PParamsUpdate (AllegraEra c)) where
  prettyA = undefined

instance PrettyA (PParams (MaryEra c)) where
  prettyA = undefined

instance PrettyA (PParamsUpdate (MaryEra c)) where
  prettyA = undefined
