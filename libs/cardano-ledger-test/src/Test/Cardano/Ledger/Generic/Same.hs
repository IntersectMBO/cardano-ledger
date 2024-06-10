{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A Proof parameterized equality test, that records the 'sameness' of
--   individual record fields, this way it is possible to know where the
--   equality failed.
module Test.Cardano.Ledger.Generic.Same where

import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..))
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..))
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Tx
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (Genesis))
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.SafeHash (SafeToHash)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTxError)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..))
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  StashedAVVMAddresses,
  UTxOState (..),
  VState (..),
  curPParamsEpochStateL,
  prevPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.Translation ()
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Foldable (toList)
import Lens.Micro ((^.))
import Prettyprinter (Doc, indent, viaShow, vsep)
import Test.Cardano.Ledger.Generic.PrettyCore
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Shelley.Examples.Consensus (
  ShelleyLedgerExamples (..),
  ShelleyResultExamples (..),
 )
import Test.Cardano.Ledger.TerseTools

-- ===========================

instance Terse (KeyHash 'Genesis c) where
  terse x = show (pcKeyHash x)

instance Terse (PParamsUpdate (ShelleyEra c)) where
  terse x = show x

instance Terse (PParamsUpdate (AllegraEra c)) where
  terse x = show x

instance Terse (PParamsUpdate (MaryEra c)) where
  terse x = show x

instance Terse (PParamsUpdate (AlonzoEra c)) where
  terse x = show x

instance Terse (PParamsUpdate (BabbageEra c)) where
  terse x = show x

instance Terse (PParamsUpdate (ConwayEra c)) where
  terse x = show x

-- ========================================
-- Helper functions

-- | Relabel by appending 's' to the front of the path
extendLabel :: String -> [(String, Maybe x)] -> [(String, Maybe x)]
extendLabel _ [] = []
extendLabel s ((n, x) : xs) = (s ++ n, x) : extendLabel s xs

-- | Dispays a difference vertically as
--   x
--     =/=
--   y
notEq :: Doc a -> Doc a -> Doc a
notEq x y = vsep [x, indent 3 (text "=/="), y]

-- | Compare for equality, and display difference using 'show'
eqByShow :: (Eq t, Show t) => t -> t -> Maybe PDoc
eqByShow x y = if x == y then Nothing else Just (notEq (ppString (show x)) (ppString (show y)))

-- | Compare for equality, and display differences using 'pcf'
eqVia :: Eq t => (t -> PDoc) -> t -> t -> Maybe PDoc
eqVia pcf x y = if x == y then Nothing else Just (notEq (pcf x) (pcf y))

-- ==========================================

-- | The Same class is similar to Eq but returns descriptions (paths and
--   non-matching components) for each difference in a large structure.
class Same era t where
  same :: Proof era -> t -> t -> [(String, Maybe PDoc)]

instance Same era (CertState era) where
  same proof (CertState d1 p1 v1) (CertState d2 p2 v2) =
    extendLabel "DState " (same proof d1 d2)
      ++ extendLabel "PState " (same proof p1 p2)
      ++ extendLabel "VState " (same proof v1 v2)

instance Same era (PState era) where
  same _proof (PState pp1 fpp1 ret1 d1) (PState pp2 fpp2 ret2 d2) =
    [ ("PoolParams", eqByShow pp1 pp2)
    , ("FuturePoolParams", eqByShow fpp1 fpp2)
    , ("Retiring", eqByShow ret1 ret2)
    , ("Deposits", eqByShow d1 d2)
    ]

instance Same era (DState era) where
  same _proof (DState u1 fgd1 gd1 ir1) (DState u2 fgd2 gd2 ir2) =
    [ ("Unified", eqByShow u1 u2)
    , ("FutureGenDelegs", eqByShow fgd1 fgd2)
    , ("GenDelegs", eqByShow gd1 gd2)
    , ("InstantaneousRewards", eqByShow ir1 ir2)
    ]

instance Same era (VState era) where
  same _proof (VState dr1 cchk1 numDE1) (VState dr2 cchk2 numDE2) =
    [ ("DReps", eqByShow dr1 dr2)
    , ("CC Hot Keys", eqByShow cchk1 cchk2)
    , ("Num Dormant Epochs", eqByShow numDE1 numDE2)
    ]

sameUTxO :: Proof era -> UTxO era -> UTxO era -> Maybe PDoc
sameUTxO Shelley x y = eqByShow x y
sameUTxO Allegra x y = eqByShow x y
sameUTxO Mary x y = eqByShow x y
sameUTxO Alonzo x y = eqByShow x y
sameUTxO Babbage x y = eqByShow x y
sameUTxO Conway x y = eqByShow x y
{-# NOINLINE sameUTxO #-}

samePPUP :: Proof era -> ShelleyGovState era -> ShelleyGovState era -> Maybe PDoc
samePPUP Shelley x y = eqByShow x y
samePPUP Allegra x y = eqByShow x y
samePPUP Mary x y = eqByShow x y
samePPUP Alonzo x y = eqByShow x y
samePPUP Babbage x y = eqByShow x y
samePPUP Conway x y = eqByShow x y
{-# NOINLINE samePPUP #-}

instance Reflect era => Same era (UTxOState era) where
  same proof u1 u2 =
    [ ("UTxO", sameUTxO proof (utxosUtxo u1) (utxosUtxo u2))
    , ("Deposited", eqByShow (utxosDeposited u1) (utxosDeposited u2))
    , ("Fees", eqByShow (utxosFees u1) (utxosFees u2))
    ]
      ++ ppu
      ++ [("StakeDistr", eqByShow (utxosStakeDistr u1) (utxosStakeDistr u2))]
    where
      ppuPretty :: GovState era ~ ShelleyGovState era => [(String, Maybe PDoc)]
      ppuPretty = [("ShelleyGovState", samePPUP proof (utxosGovState u1) (utxosGovState u2))]
      ppu = case reify @era of
        Shelley -> ppuPretty
        Mary -> ppuPretty
        Allegra -> ppuPretty
        Alonzo -> ppuPretty
        Babbage -> ppuPretty
        Conway -> []

instance Reflect era => Same era (LedgerState era) where
  same proof x1 x2 =
    extendLabel "UTxOState " (same proof (lsUTxOState x1) (lsUTxOState x2))
      ++ extendLabel "CertState " (same proof (lsCertState x1) (lsCertState x2))

instance Reflect era => Same era (EpochState era) where
  same proof e1 e2 =
    [ ("AccountState", eqByShow (esAccountState e1) (esAccountState e2))
    , ("SnapShots", eqByShow (esSnapshots e1) (esSnapshots e2))
    , ("PrevPP", samePParams proof (e1 ^. prevPParamsEpochStateL) (e2 ^. prevPParamsEpochStateL))
    , ("CurPP", samePParams proof (e1 ^. curPParamsEpochStateL) (e2 ^. curPParamsEpochStateL))
    , ("NonMyopic", eqByShow (esNonMyopic e1) (esNonMyopic e2))
    ]
      ++ extendLabel "LedgerState " (same proof (esLState e1) (esLState e2))

sameStashedAVVMAddresses ::
  Proof era -> StashedAVVMAddresses era -> StashedAVVMAddresses era -> Maybe PDoc
sameStashedAVVMAddresses proof x y =
  case proof of
    Shelley -> if x == y then Nothing else Just (viaShow x)
    Allegra -> if x == y then Nothing else Just (viaShow x)
    Mary -> if x == y then Nothing else Just (viaShow x)
    Alonzo -> if x == y then Nothing else Just (viaShow x)
    Babbage -> if x == y then Nothing else Just (viaShow x)
    Conway -> if x == y then Nothing else Just (viaShow x)

instance
  Reflect era =>
  Same era (NewEpochState era)
  where
  same proof n1 n2 =
    [ ("nesEL", eqByShow (nesEL n1) (nesEL n2))
    , ("nesBprev", eqByShow (nesBprev n1) (nesBprev n2))
    , ("nesBcur", eqByShow (nesBcur n1) (nesBcur n2))
    , ("nesRU", eqByShow (nesRu n1) (nesRu n2))
    , ("nesPd", eqByShow (nesPd n1) (nesPd n2))
    , ("nesStashAVVM", sameStashedAVVMAddresses proof (stashedAVVMAddresses n1) (stashedAVVMAddresses n2))
    ]
      ++ extendLabel "EpochState " (same proof (nesEs n1) (nesEs n2))

data SomeDepend where
  SomeD :: String -> (x -> x -> Maybe PDoc) -> x -> x -> SomeDepend
  SomeM :: String -> (x -> x -> [(String, Maybe PDoc)]) -> x -> x -> SomeDepend

-- | If x occurs in y then a difference in x forces a difference in y,
--   so only return the information on x if it has differences.
sameWithDependency :: [SomeDepend] -> [(String, Maybe PDoc)]
sameWithDependency [] = []
sameWithDependency (SomeD labx actx x1 x2 : more) =
  case actx x1 x2 of
    Nothing -> sameWithDependency more
    ansx -> (labx, ansx) : sameWithDependency more
sameWithDependency (SomeM labx actx x1 x2 : more) =
  case actx x1 x2 of
    [] -> sameWithDependency more
    ansx -> extendLabel (labx ++ " ") ansx ++ sameWithDependency more

instance
  Reflect era =>
  Same era (ShelleyLedgerExamples era)
  where
  same proof x1 x2 = case (sleBlock x1, sleBlock x2) of
    (Block' h1 a1 _, Block' h2 a2 _) ->
      sameWithDependency
        [ SomeM "Tx" (sameTx proof) (sleTx x1) (sleTx x2)
        , SomeM "TxSeq" (sameTxSeq proof) a1 a2
        ]
        ++ [ ("BlockHeader", if h1 == h2 then Nothing else Just ("UnequalBlockHeader"))
           ,
             ( "HashHeader"
             , if (sleHashHeader x1) == (sleHashHeader x2)
                then Nothing
                else Just ("UnequalHashHeader")
             )
           , ("ApplyTxError", sameLedgerFail proof (sleApplyTxError x1) (sleApplyTxError x2))
           , ("RewardsCredentials", eqByShow (sleRewardsCredentials x1) (sleRewardsCredentials x2))
           ]
        ++ extendLabel "Result " (same proof (sleResultExamples x1) (sleResultExamples x2))
        ++ extendLabel "NewEpochState " (same proof (sleNewEpochState x1) (sleNewEpochState x2))
        ++ [ ("ChainDepState", eqByShow (sleChainDepState x1) (sleChainDepState x2))
           , ("TranslationContext", sameTransCtx proof (sleTranslationContext x1) (sleTranslationContext x2))
           ]

instance Era era => Same era (ShelleyResultExamples era) where
  same proof r1 r2 =
    [ ("PParams", samePParams proof (srePParams r1) (srePParams r2))
    ,
      ( "ProposedPPUpdates"
      , case proof of
          Shelley -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Allegra -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Mary -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Alonzo -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Babbage -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Conway -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
      )
    , ("poolDistr", eqByShow (srePoolDistr r1) (srePoolDistr r2))
    , ("NonMyopicRewards", eqByShow (sreNonMyopicRewards r1) (sreNonMyopicRewards r2))
    , ("ShelleyGenesis", eqByShow (sreShelleyGenesis r1) (sreShelleyGenesis r2))
    ]
    where
      getmap (ProposedPPUpdates x) = x
      sameProposedPPUpdates x y = ppDiff $ mapdiffs (getmap x) (getmap y)
        where
          ppDiff [] = Nothing
          ppDiff xs = Just (ppString (show xs))

-- =========================================================================
-- Functions like 'same' from the 'Same' class, but which apply to type families
-- We cannot make them Same instances because they are type families.
-- We also can avoid all extra constraints by pattern matching against all current Proofs.

samePParams :: Proof era -> PParams era -> PParams era -> Maybe PDoc
samePParams Shelley x y = eqByShow x y
samePParams Allegra x y = eqByShow x y
samePParams Mary x y = eqByShow x y
samePParams Alonzo x y = eqByShow x y
samePParams Babbage x y = eqByShow x y
samePParams Conway x y = eqByShow x y
{-# NOINLINE samePParams #-}

samePParamsUpdate :: Proof era -> PParamsUpdate era -> PParamsUpdate era -> Maybe PDoc
samePParamsUpdate Shelley x y = eqByShow x y
samePParamsUpdate Allegra x y = eqByShow x y
samePParamsUpdate Mary x y = eqByShow x y
samePParamsUpdate Alonzo x y = eqByShow x y
samePParamsUpdate Babbage x y = eqByShow x y
samePParamsUpdate Conway x y = eqByShow x y
{-# NOINLINE samePParamsUpdate #-}

sameTxOut :: Proof era -> TxOut era -> TxOut era -> Maybe PDoc
sameTxOut Shelley x y = eqByShow x y
sameTxOut Allegra x y = eqByShow x y
sameTxOut Mary x y = eqByShow x y
sameTxOut Alonzo x y = eqByShow x y
sameTxOut Babbage x y = eqByShow x y
sameTxOut Conway x y = eqByShow x y
{-# NOINLINE sameTxOut #-}

sameLedgerFail ::
  Proof era ->
  ApplyTxError era ->
  ApplyTxError era ->
  Maybe PDoc
sameLedgerFail Shelley x y = eqByShow x y
sameLedgerFail Allegra x y = eqByShow x y
sameLedgerFail Mary x y = eqByShow x y
sameLedgerFail Alonzo x y = eqByShow x y
sameLedgerFail Babbage x y = eqByShow x y
sameLedgerFail Conway x y = eqByShow x y
{-# NOINLINE sameLedgerFail #-}

sameTransCtx ::
  Proof era ->
  TranslationContext era ->
  TranslationContext era ->
  Maybe PDoc
sameTransCtx Shelley x y = eqByShow x y
sameTransCtx Allegra x y = eqByShow x y
sameTransCtx Mary x y = eqByShow x y
sameTransCtx Alonzo x y = eqByShow x y
sameTransCtx Babbage x y = eqByShow x y
sameTransCtx Conway x y = eqByShow x y
{-# NOINLINE sameTransCtx #-}

-- ==========================
-- Comparing witnesses for Sameness

sameShelleyTxWits ::
  forall era.
  Reflect era =>
  Proof era ->
  ShelleyTxWits era ->
  ShelleyTxWits era ->
  [(String, Maybe PDoc)]
sameShelleyTxWits proof (ShelleyTxWits vk1 sh1 boot1) (ShelleyTxWits vk2 sh2 boot2) =
  [ ("VKeyWits", eqVia (ppSet (pcWitVKey proof)) vk1 vk2)
  , ("ScriptWits", eqVia (ppMap pcScriptHash (pcScript proof)) sh1 sh2)
  , ("BootWits", eqVia (\_ -> ppString "BOOTWITS") boot1 boot2)
  ]

sameAlonzoTxWits ::
  forall era.
  (Reflect era, AlonzoEraScript era) =>
  Proof era ->
  AlonzoTxWits era ->
  AlonzoTxWits era ->
  [(String, Maybe PDoc)]
sameAlonzoTxWits
  proof
  (AlonzoTxWits vk1 boot1 sh1 (TxDats d1) (Redeemers r1))
  (AlonzoTxWits vk2 boot2 sh2 (TxDats d2) (Redeemers r2)) =
    [ ("VKeyWits", eqVia (ppSet (pcWitVKey proof)) vk1 vk2)
    , ("BootWits", eqVia (\_ -> ppString "BOOTWITS") boot1 boot2)
    , ("ScriptWits", eqVia (ppMap pcScriptHash (pcScript proof)) sh1 sh2)
    , ("DataWits", eqVia (ppMap pcDataHash pcData) d1 d2)
    , ("RedeemerWits", eqVia (ppMap ppPlutusPurposeAsIx (pcPair pcData pcExUnits)) r1 r2)
    ]

sameTxWits :: Reflect era => Proof era -> TxWits era -> TxWits era -> [(String, Maybe PDoc)]
sameTxWits proof@Shelley x y = sameShelleyTxWits proof x y
sameTxWits proof@Allegra x y = sameShelleyTxWits proof x y
sameTxWits proof@Mary x y = sameShelleyTxWits proof x y
sameTxWits proof@Alonzo x y = sameAlonzoTxWits proof x y
sameTxWits proof@Babbage x y = sameAlonzoTxWits proof x y
sameTxWits proof@Conway x y = sameAlonzoTxWits proof x y

-- =======================
-- Comparing TxBody for Sameness

sameShelleyTxBody ::
  Reflect era =>
  Proof era ->
  ShelleyTxBody era ->
  ShelleyTxBody era ->
  [(String, Maybe PDoc)]
sameShelleyTxBody proof (ShelleyTxBody i1 o1 c1 (Withdrawals w1) f1 s1 pu1 d1) (ShelleyTxBody i2 o2 c2 (Withdrawals w2) f2 s2 pu2 d2) =
  [ ("Inputs", eqVia (ppSet pcTxIn) i1 i2)
  , ("Outputs", eqVia (ppList (pcTxOut proof) . toList) o1 o2)
  , ("TxCert", eqVia (ppList (pcTxCert proof) . toList) c1 c2)
  , ("WDRL", eqVia (ppMap pcRewardAccount pcCoin) w1 w2)
  , ("Fee", eqVia pcCoin f1 f2)
  , ("TimeToLive", eqVia pcSlotNo s1 s2)
  , ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2)
  , ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2)
  ]

sameAllegraTxBody ::
  Reflect era =>
  Proof era ->
  AllegraTxBody era ->
  AllegraTxBody era ->
  [(String, Maybe PDoc)]
sameAllegraTxBody proof (AllegraTxBody i1 o1 c1 (Withdrawals w1) f1 v1 pu1 d1) (AllegraTxBody i2 o2 c2 (Withdrawals w2) f2 v2 pu2 d2) =
  [ ("Inputs", eqVia (ppSet pcTxIn) i1 i2)
  , ("Outputs", eqVia (ppList (pcTxOut proof) . toList) o1 o2)
  , ("TxCert", eqVia (ppList (pcTxCert proof) . toList) c1 c2)
  , ("WDRL", eqVia (ppMap pcRewardAccount pcCoin) w1 w2)
  , ("Fee", eqVia pcCoin f1 f2)
  , ("ValidityInterval", eqVia ppValidityInterval v1 v2)
  , ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2)
  , ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2)
  ]

sameMaryTxBody ::
  Reflect era =>
  Proof era ->
  MaryTxBody era ->
  MaryTxBody era ->
  [(String, Maybe PDoc)]
sameMaryTxBody proof (MaryTxBody i1 o1 c1 (Withdrawals w1) f1 v1 pu1 d1 m1) (MaryTxBody i2 o2 c2 (Withdrawals w2) f2 v2 pu2 d2 m2) =
  [ ("Inputs", eqVia (ppSet pcTxIn) i1 i2)
  , ("Outputs", eqVia (ppList (pcTxOut proof) . toList) o1 o2)
  , ("TxCert", eqVia (ppList (pcTxCert proof) . toList) c1 c2)
  , ("WDRL", eqVia (ppMap pcRewardAccount pcCoin) w1 w2)
  , ("Fee", eqVia pcCoin f1 f2)
  , ("ValidityInterval", eqVia ppValidityInterval v1 v2)
  , ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2)
  , ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2)
  , ("Mint", eqVia multiAssetSummary m1 m2)
  ]

sameAlonzoTxBody ::
  Reflect era =>
  Proof era ->
  AlonzoTxBody era ->
  AlonzoTxBody era ->
  [(String, Maybe PDoc)]
sameAlonzoTxBody
  proof
  (AlonzoTxBody i1 cl1 o1 c1 (Withdrawals w1) f1 v1 pu1 r1 m1 s1 d1 n1)
  (AlonzoTxBody i2 cl2 o2 c2 (Withdrawals w2) f2 v2 pu2 r2 m2 s2 d2 n2) =
    [ ("Inputs", eqVia (ppSet pcTxIn) i1 i2)
    , ("Collateral", eqVia (ppSet pcTxIn) cl1 cl2)
    , ("Outputs", eqVia (ppList (pcTxOut proof) . toList) o1 o2)
    , ("Certs", eqVia (ppList (pcTxCert proof) . toList) c1 c2)
    , ("WDRL", eqVia (ppMap pcRewardAccount pcCoin) w1 w2)
    , ("Fee", eqVia pcCoin f1 f2)
    , ("ValidityInterval", eqVia ppValidityInterval v1 v2)
    , ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2)
    , ("ReqSignerHashes", eqVia (ppSet pcKeyHash) r1 r2)
    , ("Mint", eqVia multiAssetSummary m1 m2)
    , ("ScriptIntegrityHash", eqVia (ppStrictMaybe (trim . ppSafeHash)) s1 s2)
    , ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2)
    , ("NetworkId", eqVia (ppStrictMaybe pcNetwork) n1 n2)
    ]

sameBabbageTxBody ::
  ( Reflect era
  , BabbageEraTxBody era
  ) =>
  Proof era ->
  BabbageTxBody era ->
  BabbageTxBody era ->
  [(String, Maybe PDoc)]
sameBabbageTxBody
  proof
  (BabbageTxBody i1 cl1 ri1 o1 cr1 tc1 c1 (Withdrawals w1) f1 v1 pu1 r1 m1 s1 d1 n1)
  (BabbageTxBody i2 cl2 ri2 o2 cr2 tc2 c2 (Withdrawals w2) f2 v2 pu2 r2 m2 s2 d2 n2) =
    [ ("SpendInputs", eqVia (ppSet pcTxIn) i1 i2)
    , ("ColInputs", eqVia (ppSet pcTxIn) cl1 cl2)
    , ("RefInputs", eqVia (ppSet pcTxIn) ri1 ri2)
    , ("Outputs", eqVia (ppList (pcTxOut proof . sizedValue) . toList) o1 o2)
    , ("ColReturn", eqVia (ppStrictMaybe (pcTxOut proof . sizedValue)) cr1 cr2)
    , ("TotalCol", eqVia (ppStrictMaybe pcCoin) tc1 tc2)
    , ("Certs", eqVia (ppList (pcTxCert proof) . toList) c1 c2)
    , ("WDRL", eqVia (ppMap pcRewardAccount pcCoin) w1 w2)
    , ("Fee", eqVia pcCoin f1 f2)
    , ("ValidityInterval", eqVia ppValidityInterval v1 v2)
    , ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2)
    , ("ReqSignerHashes", eqVia (ppSet pcKeyHash) r1 r2)
    , ("Mint", eqVia multiAssetSummary m1 m2)
    , ("ScriptIntegrityHash", eqVia (ppStrictMaybe (trim . ppSafeHash)) s1 s2)
    , ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2)
    , ("NetworkId", eqVia (ppStrictMaybe pcNetwork) n1 n2)
    ]

sameConwayTxBody ::
  ( ConwayEraTxBody era
  , Reflect era
  ) =>
  Proof era ->
  ConwayTxBody era ->
  ConwayTxBody era ->
  [(String, Maybe PDoc)]
sameConwayTxBody
  -- TODO WG add new stuff in here
  proof
  (ConwayTxBody i1 cl1 ri1 o1 cr1 tc1 c1 (Withdrawals w1) f1 v1 r1 m1 s1 d1 n1 vp1 pp1 ctv1 td1 _ _ _)
  (ConwayTxBody i2 cl2 ri2 o2 cr2 tc2 c2 (Withdrawals w2) f2 v2 r2 m2 s2 d2 n2 vp2 pp2 ctv2 td2 _ _ _) =
    [ ("SpendInputs", eqVia (ppSet pcTxIn) i1 i2)
    , ("ColInputs", eqVia (ppSet pcTxIn) cl1 cl2)
    , ("RefInputs", eqVia (ppSet pcTxIn) ri1 ri2)
    , ("Outputs", eqVia (ppList (pcTxOut proof . sizedValue) . toList) o1 o2)
    , ("ColReturn", eqVia (ppStrictMaybe (pcTxOut proof . sizedValue)) cr1 cr2)
    , ("TotalCol", eqVia (ppStrictMaybe pcCoin) tc1 tc2)
    , ("Certs", eqVia (ppList pcConwayTxCert . toList) c1 c2)
    , ("WDRL", eqVia (ppMap pcRewardAccount pcCoin) w1 w2)
    , ("Fee", eqVia pcCoin f1 f2)
    , ("ValidityInterval", eqVia ppValidityInterval v1 v2)
    , ("ReqSignerHashes", eqVia (ppSet pcKeyHash) r1 r2)
    , ("Mint", eqVia multiAssetSummary m1 m2)
    , ("ScriptIntegrityHash", eqVia (ppStrictMaybe (trim . ppSafeHash)) s1 s2)
    , ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2)
    , ("NetworkId", eqVia (ppStrictMaybe pcNetwork) n1 n2)
    ,
      ( "VotingProcedures"
      , eqVia
          (ppMap pcVoter (ppMap pcGovActionId pcVotingProcedure))
          (unVotingProcedures vp1)
          (unVotingProcedures vp2)
      )
    , ("ProposalProcedures", eqVia (ppOSet pcProposalProcedure) pp1 pp2)
    , ("CurrentTreasuryValue", eqVia (ppStrictMaybe pcCoin) ctv1 ctv2)
    , ("TreasuryDonation", eqVia pcCoin td1 td2)
    ]

sameTxBody :: Reflect era => Proof era -> TxBody era -> TxBody era -> [(String, Maybe PDoc)]
sameTxBody proof@Shelley x y = sameShelleyTxBody proof x y
sameTxBody proof@Allegra x y = sameAllegraTxBody proof x y
sameTxBody proof@Mary x y = sameMaryTxBody proof x y
sameTxBody proof@Alonzo x y = sameAlonzoTxBody proof x y
sameTxBody proof@Babbage x y = sameBabbageTxBody proof x y
sameTxBody proof@Conway x y = sameConwayTxBody proof x y

-- =======================
-- Comparing Tx for Sameness

sameShelleyTx ::
  (Reflect era, TxWits era ~ ShelleyTxWits era) =>
  Proof era ->
  ShelleyTx era ->
  ShelleyTx era ->
  [(String, Maybe PDoc)]
sameShelleyTx proof (ShelleyTx b1 w1 aux1) (ShelleyTx b2 w2 aux2) =
  extendLabel "TxBody " (sameTxBody proof b1 b2)
    ++ extendLabel "TxWits " (sameShelleyTxWits proof w1 w2)
    ++ [ ("AuxData", eqByShow aux1 aux2)
       ]

sameAlonzoTx ::
  ( Reflect era
  , AlonzoEraScript era
  , TxWits era ~ AlonzoTxWits era
  ) =>
  Proof era ->
  AlonzoTx era ->
  AlonzoTx era ->
  [(String, Maybe PDoc)]
-- TODO WG add new stuff in here
sameAlonzoTx proof (AlonzoTx b1 w1 v1 aux1) (AlonzoTx b2 w2 v2 aux2) =
  extendLabel "TxBody " (sameTxBody proof b1 b2)
    ++ extendLabel "TxWits " (sameAlonzoTxWits proof w1 w2)
    ++ [ ("AuxData", eqByShow aux1 aux2)
       , ("IsValid", eqByShow v1 v2)
       ]
{-# NOINLINE sameAlonzoTx #-}

sameTx :: Reflect era => Proof era -> Tx era -> Tx era -> [(String, Maybe PDoc)]
sameTx proof@Shelley x y = sameShelleyTx proof x y
sameTx proof@Allegra x y = sameShelleyTx proof x y
sameTx proof@Mary x y = sameShelleyTx proof x y
sameTx proof@Alonzo x y = sameAlonzoTx proof x y
sameTx proof@Babbage x y = sameAlonzoTx proof x y
sameTx proof@Conway x y = sameAlonzoTx proof x y
{-# NOINLINE sameTx #-}

-- ==========================
-- Comparing TxSeq for Sameness

ints :: [Int]
ints = [0 ..]

sameShelleyTxSeq ::
  ( Reflect era
  , Tx era ~ ShelleyTx era
  , SafeToHash (TxWits era)
  ) =>
  Proof era ->
  ShelleyTxSeq era ->
  ShelleyTxSeq era ->
  [(String, Maybe PDoc)]
sameShelleyTxSeq proof (ShelleyTxSeq ss1) (ShelleyTxSeq ss2) =
  sameWithDependency (zipWith3 f ints (toList ss1) (toList ss2))
  where
    f n t1 t2 = SomeM (show n) (sameTx proof) t1 t2

sameAlonzoTxSeq ::
  ( Reflect era
  , AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  Proof era ->
  AlonzoTxSeq era ->
  AlonzoTxSeq era ->
  [(String, Maybe PDoc)]
sameAlonzoTxSeq proof (AlonzoTxSeq ss1) (AlonzoTxSeq ss2) =
  sameWithDependency (zipWith3 f ints (toList ss1) (toList ss2))
  where
    f n t1 t2 = SomeM (show n) (sameTx proof) t1 t2

sameConwayTxZones ::
  ( Reflect era
  , AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  Proof era ->
  ConwayTxZones era ->
  ConwayTxZones era ->
  [(String, Maybe PDoc)]
sameConwayTxZones proof (ConwayTxZones ss1) (ConwayTxZones ss2) =
  sameWithDependency (zipWith3 f ints (concatMap toList ss1) (concatMap toList ss2))
  where
    f n t1 t2 = SomeM (show n) (sameTx proof) t1 t2

sameTxSeq :: Reflect era => Proof era -> TxZones era -> TxZones era -> [(String, Maybe PDoc)]
sameTxSeq proof@Shelley x y = sameShelleyTxSeq proof x y
sameTxSeq proof@Allegra x y = sameShelleyTxSeq proof x y
sameTxSeq proof@Mary x y = sameShelleyTxSeq proof x y
sameTxSeq proof@Alonzo x y = sameAlonzoTxSeq proof x y
sameTxSeq proof@Babbage x y = sameAlonzoTxSeq proof x y
sameTxSeq proof@Conway x y = sameConwayTxZones proof x y
{-# NOINLINE sameTxSeq #-}
