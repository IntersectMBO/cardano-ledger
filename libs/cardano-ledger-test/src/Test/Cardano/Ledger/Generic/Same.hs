{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A Proof parameterized equality test, that records the 'sameness' of
--   individual record fields, this way it is possible to know where the
--   equality failed.
module Test.Cardano.Ledger.Generic.Same where

import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Alonzo.PParams (AlonzoPParamsHKD)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..))
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD)
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody, BabbageTxBody (..), BabbageTxOut (..))
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Block (Block (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era, EraCrypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (Genesis))
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Alonzo (ppRdmrPtr)
import Cardano.Ledger.Pretty.Mary (ppValidityInterval)
import Cardano.Ledger.SafeHash (SafeToHash)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTxError)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..))
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    StashedAVVMAddresses,
    UTxOState (..),
  )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), ShelleyPParamsHKD)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..), ShelleyTxOut, Wdrl (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Control.State.Transition.Extended (State)
import Data.Foldable (toList)
import Data.Maybe.Strict (StrictMaybe)
import Prettyprinter (Doc, indent, viaShow, vsep)
import Test.Cardano.Ledger.Generic.PrettyCore
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Shelley.Examples.Consensus
  ( ShelleyLedgerExamples (..),
    ShelleyResultExamples (..),
  )
import Test.Cardano.Ledger.TerseTools

-- ===========================

instance Terse (KeyHash 'Genesis c) where
  terse x = show (pcKeyHash x)

instance Terse (ShelleyPParamsHKD StrictMaybe c) where
  terse x = show x

instance Terse (AlonzoPParamsHKD StrictMaybe c) where
  terse x = show x

instance Terse (BabbagePParamsHKD StrictMaybe c) where
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
eqVia :: (Eq t) => (t -> PDoc) -> t -> t -> Maybe PDoc
eqVia pcf x y = if x == y then Nothing else Just (notEq (pcf x) (pcf y))

-- ==========================================

-- | The Same class is similar to Eq but returns descriptions (paths and
--   non-matching components) for each difference in a large structure.
class Same era t where
  same :: Proof era -> t -> t -> [(String, Maybe PDoc)]

instance (EraCrypto era ~ c) => Same era (DPState c) where
  same proof (DPState d1 p1) (DPState d2 p2) =
    extendLabel "DState " (same proof d1 d2) ++ extendLabel "PState " (same proof p1 p2)

instance (EraCrypto era ~ c) => Same era (PState c) where
  same _proof (PState pp1 fpp1 ret1) (PState pp2 fpp2 ret2) =
    [ ("PoolParams", eqByShow pp1 pp2),
      ("FuturePoolParams", eqByShow fpp1 fpp2),
      ("Retiring", eqByShow ret1 ret2)
    ]

instance (EraCrypto era ~ c) => Same era (DState c) where
  same _proof (DState u1 fgd1 gd1 ir1) (DState u2 fgd2 gd2 ir2) =
    [ ("Unified", eqByShow u1 u2),
      ("FutureGenDelegs", eqByShow fgd1 fgd2),
      ("GenDelegs", eqByShow gd1 gd2),
      ("InstantaneousRewards", eqByShow ir1 ir2)
    ]

sameUTxO :: Proof era -> UTxO era -> UTxO era -> Maybe PDoc
sameUTxO (Shelley _) x y = eqByShow x y
sameUTxO (Allegra _) x y = eqByShow x y
sameUTxO (Mary _) x y = eqByShow x y
sameUTxO (Alonzo _) x y = eqByShow x y
sameUTxO (Babbage _) x y = eqByShow x y
sameUTxO (Conway _) x y = eqByShow x y
{-# NOINLINE sameUTxO #-}

samePPUP :: Proof era -> State (Core.EraRule "PPUP" era) -> State (Core.EraRule "PPUP" era) -> Maybe PDoc
samePPUP (Shelley _) x y = eqByShow x y
samePPUP (Allegra _) x y = eqByShow x y
samePPUP (Mary _) x y = eqByShow x y
samePPUP (Alonzo _) x y = eqByShow x y
samePPUP (Babbage _) x y = eqByShow x y
samePPUP (Conway _) x y = eqByShow x y
{-# NOINLINE samePPUP #-}

instance (Era era) => Same era (UTxOState era) where
  same proof u1 u2 =
    [ ("UTxO", sameUTxO proof (utxosUtxo u1) (utxosUtxo u2)),
      ("Deposited", eqByShow (utxosDeposited u1) (utxosDeposited u2)),
      ("Fees", eqByShow (utxosFees u1) (utxosFees u2)),
      ("PPUpdates", samePPUP proof (utxosPpups u1) (utxosPpups u2)),
      ("StakeDistr", eqByShow (utxosStakeDistr u1) (utxosStakeDistr u2))
    ]

instance (Era era) => Same era (LedgerState era) where
  same proof x1 x2 =
    extendLabel "UTxOState " (same proof (lsUTxOState x1) (lsUTxOState x2))
      ++ extendLabel "DPState " (same proof (lsDPState x1) (lsDPState x2))

instance (Era era) => Same era (EpochState era) where
  same proof e1 e2 =
    [ ("AccountState", eqByShow (esAccountState e1) (esAccountState e2)),
      ("SnapShots", eqByShow (esSnapshots e1) (esSnapshots e2)),
      ("PrevPP", samePParams proof (esPrevPp e1) (esPrevPp e2)),
      ("CurPP", samePParams proof (esPp e1) (esPp e2)),
      ("NonMyopic", eqByShow (esNonMyopic e1) (esNonMyopic e2))
    ]
      ++ extendLabel "LedgerState " (same proof (esLState e1) (esLState e2))

sameStashedAVVMAddresses ::
  Proof era -> StashedAVVMAddresses era -> StashedAVVMAddresses era -> Maybe PDoc
sameStashedAVVMAddresses proof x y =
  case proof of
    Shelley _ -> if x == y then Nothing else Just (viaShow x)
    Allegra _ -> if x == y then Nothing else Just (viaShow x)
    Mary _ -> if x == y then Nothing else Just (viaShow x)
    Alonzo _ -> if x == y then Nothing else Just (viaShow x)
    Babbage _ -> if x == y then Nothing else Just (viaShow x)
    Conway _ -> if x == y then Nothing else Just (viaShow x)

instance
  ( Era era
  ) =>
  Same era (NewEpochState era)
  where
  same proof n1 n2 =
    [ ("nesEL", eqByShow (nesEL n1) (nesEL n2)),
      ("nesBprev", eqByShow (nesBprev n1) (nesBprev n2)),
      ("nesBcur", eqByShow (nesBcur n1) (nesBcur n2)),
      ("nesRU", eqByShow (nesRu n1) (nesRu n2)),
      ("nesPd", eqByShow (nesPd n1) (nesPd n2)),
      ("nesStashAVVM", sameStashedAVVMAddresses proof (stashedAVVMAddresses n1) (stashedAVVMAddresses n2))
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

instance Reflect era => Same era (ShelleyLedgerExamples era) where
  same proof x1 x2 = case (sleBlock x1, sleBlock x2) of
    (Block' h1 a1 _, Block' h2 a2 _) ->
      sameWithDependency
        [ SomeM "Tx" (sameTx proof) (sleTx x1) (sleTx x2),
          SomeM "TxSeq" (sameTxSeq proof) a1 a2
        ]
        ++ [ ("BlockHeader", if h1 == h2 then Nothing else Just ("UnequalBlockHeader")),
             ( "HashHeader",
               if (sleHashHeader x1) == (sleHashHeader x2)
                 then Nothing
                 else Just ("UnequalHashHeader")
             ),
             ("ApplyTxError", sameLedgerFail proof (sleApplyTxError x1) (sleApplyTxError x2)),
             ("RewardsCredentials", eqByShow (sleRewardsCredentials x1) (sleRewardsCredentials x2))
           ]
        ++ extendLabel "Result " (same proof (sleResultExamples x1) (sleResultExamples x2))
        ++ extendLabel "NewEpochState " (same proof (sleNewEpochState x1) (sleNewEpochState x2))
        ++ [ ("ChainDepState", eqByShow (sleChainDepState x1) (sleChainDepState x2)),
             ("TranslationContext", sameTransCtx proof (sleTranslationContext x1) (sleTranslationContext x2))
           ]

instance Same era (ShelleyResultExamples era) where
  same proof r1 r2 =
    [ ("PParams", samePParams proof (srePParams r1) (srePParams r2)),
      ( "ProposedPPUpdates",
        case proof of
          Shelley _ -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Allegra _ -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Mary _ -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Alonzo _ -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Babbage _ -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
          Conway _ -> sameProposedPPUpdates (sreProposedPPUpdates r1) (sreProposedPPUpdates r2)
      ),
      ("poolDistr", eqByShow (srePoolDistr r1) (srePoolDistr r2)),
      ("NonMyopicRewards", eqByShow (sreNonMyopicRewards r1) (sreNonMyopicRewards r2)),
      ("ShelleyGenesis", eqByShow (sreShelleyGenesis r1) (sreShelleyGenesis r2))
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

samePParams :: Proof era -> Core.PParams era -> Core.PParams era -> Maybe PDoc
samePParams (Shelley _) x y = eqByShow x y
samePParams (Allegra _) x y = eqByShow x y
samePParams (Mary _) x y = eqByShow x y
samePParams (Alonzo _) x y = eqByShow x y
samePParams (Babbage _) x y = eqByShow x y
samePParams (Conway _) x y = eqByShow x y
{-# NOINLINE samePParams #-}

samePParamsUpdate :: Proof era -> Core.PParamsUpdate era -> Core.PParamsUpdate era -> Maybe PDoc
samePParamsUpdate (Shelley _) x y = eqByShow x y
samePParamsUpdate (Allegra _) x y = eqByShow x y
samePParamsUpdate (Mary _) x y = eqByShow x y
samePParamsUpdate (Alonzo _) x y = eqByShow x y
samePParamsUpdate (Babbage _) x y = eqByShow x y
samePParamsUpdate (Conway _) x y = eqByShow x y
{-# NOINLINE samePParamsUpdate #-}

sameTxOut :: Proof era -> Core.TxOut era -> Core.TxOut era -> Maybe PDoc
sameTxOut (Shelley _) x y = eqByShow x y
sameTxOut (Allegra _) x y = eqByShow x y
sameTxOut (Mary _) x y = eqByShow x y
sameTxOut (Alonzo _) x y = eqByShow x y
sameTxOut (Babbage _) x y = eqByShow x y
sameTxOut (Conway _) x y = eqByShow x y
{-# NOINLINE sameTxOut #-}

sameLedgerFail :: Proof era -> ApplyTxError era -> ApplyTxError era -> Maybe PDoc
sameLedgerFail (Shelley _) x y = eqByShow x y
sameLedgerFail (Allegra _) x y = eqByShow x y
sameLedgerFail (Mary _) x y = eqByShow x y
sameLedgerFail (Alonzo _) x y = eqByShow x y
sameLedgerFail (Babbage _) x y = eqByShow x y
sameLedgerFail (Conway _) x y = eqByShow x y
{-# NOINLINE sameLedgerFail #-}

sameTransCtx :: Proof era -> Core.TranslationContext era -> Core.TranslationContext era -> Maybe PDoc
sameTransCtx (Shelley _) x y = eqByShow x y
sameTransCtx (Allegra _) x y = eqByShow x y
sameTransCtx (Mary _) x y = eqByShow x y
sameTransCtx (Alonzo _) x y = eqByShow x y
sameTransCtx (Babbage _) x y = eqByShow x y
sameTransCtx (Conway _) x y = eqByShow x y
{-# NOINLINE sameTransCtx #-}

-- ==========================
-- Comparing witnesses for Sameness

sameShelleyTxWits ::
  forall era.
  (Reflect era) =>
  Proof era ->
  ShelleyTxWits era ->
  ShelleyTxWits era ->
  [(String, Maybe PDoc)]
sameShelleyTxWits proof (ShelleyTxWits vk1 sh1 boot1) (ShelleyTxWits vk2 sh2 boot2) =
  [ ("VKeyWits", eqVia (ppSet (pcWitVKey @era)) vk1 vk2),
    ("ScriptWits", eqVia (ppMap pcScriptHash (pcScript proof)) sh1 sh2),
    ("BootWits", eqVia (\_ -> ppString "BOOTWITS") boot1 boot2)
  ]

sameAlonzoTxWits ::
  forall era.
  (Reflect era, Core.Script era ~ AlonzoScript era) =>
  Proof era ->
  AlonzoTxWits era ->
  AlonzoTxWits era ->
  [(String, Maybe PDoc)]
sameAlonzoTxWits
  proof
  (AlonzoTxWits vk1 boot1 sh1 (TxDats d1) (Redeemers r1))
  (AlonzoTxWits vk2 boot2 sh2 (TxDats d2) (Redeemers r2)) =
    [ ("VKeyWits", eqVia (ppSet (pcWitVKey @era)) vk1 vk2),
      ("BootWits", eqVia (\_ -> ppString "BOOTWITS") boot1 boot2),
      ("ScriptWits", eqVia (ppMap pcScriptHash (pcScript proof)) sh1 sh2),
      ("DataWits", eqVia (ppMap pcDataHash pcData) d1 d2),
      ("RedeemerWits", eqVia (ppMap ppRdmrPtr (pcPair pcData pcExUnits)) r1 r2)
    ]

sameTxWits :: Reflect era => Proof era -> Core.TxWits era -> Core.TxWits era -> [(String, Maybe PDoc)]
sameTxWits proof@(Shelley _) x y = sameShelleyTxWits proof x y
sameTxWits proof@(Allegra _) x y = sameShelleyTxWits proof x y
sameTxWits proof@(Mary _) x y = sameShelleyTxWits proof x y
sameTxWits proof@(Alonzo _) x y = sameAlonzoTxWits proof x y
sameTxWits proof@(Babbage _) x y = sameAlonzoTxWits proof x y
sameTxWits proof@(Conway _) x y = sameAlonzoTxWits proof x y

-- =======================
-- Comparing TxBody for Sameness

sameShelleyTxBody ::
  ( Reflect era,
    Core.TxOut era ~ ShelleyTxOut era
  ) =>
  Proof era ->
  ShelleyTxBody era ->
  ShelleyTxBody era ->
  [(String, Maybe PDoc)]
sameShelleyTxBody proof (ShelleyTxBody i1 o1 c1 (Wdrl w1) f1 s1 pu1 d1) (ShelleyTxBody i2 o2 c2 (Wdrl w2) f2 s2 pu2 d2) =
  [ ("Inputs", eqVia (ppSet pcTxIn) i1 i2),
    ("Outputs", eqVia (ppList (pcTxOut proof) . toList) o1 o2),
    ("DCert", eqVia (ppList pcDCert . toList) c1 c2),
    ("WDRL", eqVia (ppMap pcRewardAcnt pcCoin) w1 w2),
    ("Fee", eqVia pcCoin f1 f2),
    ("TimeToLive", eqVia ppSlotNo s1 s2),
    ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2),
    ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2)
  ]

sameMATxBody ::
  ( Reflect era,
    Core.TxOut era ~ ShelleyTxOut era
  ) =>
  Proof era ->
  MATxBody era ->
  MATxBody era ->
  [(String, Maybe PDoc)]
sameMATxBody proof (MATxBody i1 o1 c1 (Wdrl w1) f1 v1 pu1 d1 m1) (MATxBody i2 o2 c2 (Wdrl w2) f2 v2 pu2 d2 m2) =
  [ ("Inputs", eqVia (ppSet pcTxIn) i1 i2),
    ("Outputs", eqVia (ppList (pcTxOut proof) . toList) o1 o2),
    ("DCert", eqVia (ppList pcDCert . toList) c1 c2),
    ("WDRL", eqVia (ppMap pcRewardAcnt pcCoin) w1 w2),
    ("Fee", eqVia pcCoin f1 f2),
    ("ValidityInterval", eqVia ppValidityInterval v1 v2),
    ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2),
    ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2),
    ("Mint", eqVia multiAssetSummary m1 m2)
  ]

sameAlonzoTxBody ::
  Reflect era =>
  Proof era ->
  AlonzoTxBody era ->
  AlonzoTxBody era ->
  [(String, Maybe PDoc)]
sameAlonzoTxBody
  proof
  (AlonzoTxBody i1 cl1 o1 c1 (Wdrl w1) f1 v1 pu1 r1 m1 s1 d1 n1)
  (AlonzoTxBody i2 cl2 o2 c2 (Wdrl w2) f2 v2 pu2 r2 m2 s2 d2 n2) =
    [ ("Inputs", eqVia (ppSet pcTxIn) i1 i2),
      ("Collateral", eqVia (ppSet pcTxIn) cl1 cl2),
      ("Outputs", eqVia (ppList (pcTxOut proof) . toList) o1 o2),
      ("Certs", eqVia (ppList pcDCert . toList) c1 c2),
      ("WDRL", eqVia (ppMap pcRewardAcnt pcCoin) w1 w2),
      ("Fee", eqVia pcCoin f1 f2),
      ("ValidityInterval", eqVia ppValidityInterval v1 v2),
      ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2),
      ("ReqSignerHashes", eqVia (ppSet pcKeyHash) r1 r2),
      ("Mint", eqVia multiAssetSummary m1 m2),
      ("ScriptIntegrityHash", eqVia (ppStrictMaybe (trim . ppSafeHash)) s1 s2),
      ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2),
      ("NetworkId", eqVia (ppStrictMaybe pcNetwork) n1 n2)
    ]

sameBabbageTxBody ::
  ( Reflect era,
    BabbageEraTxBody era,
    Core.TxOut era ~ BabbageTxOut era
  ) =>
  Proof era ->
  BabbageTxBody era ->
  BabbageTxBody era ->
  [(String, Maybe PDoc)]
sameBabbageTxBody
  proof
  (BabbageTxBody i1 cl1 ri1 o1 cr1 tc1 c1 (Wdrl w1) f1 v1 pu1 r1 m1 s1 d1 n1)
  (BabbageTxBody i2 cl2 ri2 o2 cr2 tc2 c2 (Wdrl w2) f2 v2 pu2 r2 m2 s2 d2 n2) =
    [ ("SpendInputs", eqVia (ppSet pcTxIn) i1 i2),
      ("ColInputs", eqVia (ppSet pcTxIn) cl1 cl2),
      ("RefInputs", eqVia (ppSet pcTxIn) ri1 ri2),
      ("Outputs", eqVia (ppList (pcTxOut proof . sizedValue) . toList) o1 o2),
      ("ColReturn", eqVia (ppStrictMaybe (pcTxOut proof . sizedValue)) cr1 cr2),
      ("TotalCol", eqVia (ppStrictMaybe pcCoin) tc1 tc2),
      ("Certs", eqVia (ppList pcDCert . toList) c1 c2),
      ("WDRL", eqVia (ppMap pcRewardAcnt pcCoin) w1 w2),
      ("Fee", eqVia pcCoin f1 f2),
      ("ValidityInterval", eqVia ppValidityInterval v1 v2),
      ("PPupdate", eqVia (\_ -> ppString "Update") pu1 pu2),
      ("ReqSignerHashes", eqVia (ppSet pcKeyHash) r1 r2),
      ("Mint", eqVia multiAssetSummary m1 m2),
      ("ScriptIntegrityHash", eqVia (ppStrictMaybe (trim . ppSafeHash)) s1 s2),
      ("AuxDataHash", eqVia (ppStrictMaybe (\(AuxiliaryDataHash h) -> trim (ppSafeHash h))) d1 d2),
      ("NetworkId", eqVia (ppStrictMaybe pcNetwork) n1 n2)
    ]

sameTxBody :: Reflect era => Proof era -> Core.TxBody era -> Core.TxBody era -> [(String, Maybe PDoc)]
sameTxBody proof@(Shelley _) x y = sameShelleyTxBody proof x y
sameTxBody proof@(Allegra _) x y = sameMATxBody proof x y
sameTxBody proof@(Mary _) x y = sameMATxBody proof x y
sameTxBody proof@(Alonzo _) x y = sameAlonzoTxBody proof x y
sameTxBody proof@(Babbage _) x y = sameBabbageTxBody proof x y
sameTxBody proof@(Conway _) x y = sameBabbageTxBody proof x y

-- =======================
-- Comparing Tx for Sameness

sameShelleyTx ::
  (Reflect era, Core.TxWits era ~ ShelleyTxWits era) =>
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
  ( Reflect era,
    Core.Script era ~ AlonzoScript era,
    Core.TxWits era ~ AlonzoTxWits era
  ) =>
  Proof era ->
  AlonzoTx era ->
  AlonzoTx era ->
  [(String, Maybe PDoc)]
sameAlonzoTx proof (AlonzoTx b1 w1 v1 aux1) (AlonzoTx b2 w2 v2 aux2) =
  extendLabel "TxBody " (sameTxBody proof b1 b2)
    ++ extendLabel "TxWits " (sameAlonzoTxWits proof w1 w2)
    ++ [ ("AuxData", eqByShow aux1 aux2),
         ("IsValid", eqByShow v1 v2)
       ]
{-# NOINLINE sameAlonzoTx #-}

sameTx :: Reflect era => Proof era -> Core.Tx era -> Core.Tx era -> [(String, Maybe PDoc)]
sameTx proof@(Shelley _) x y = sameShelleyTx proof x y
sameTx proof@(Allegra _) x y = sameShelleyTx proof x y
sameTx proof@(Mary _) x y = sameShelleyTx proof x y
sameTx proof@(Alonzo _) x y = sameAlonzoTx proof x y
sameTx proof@(Babbage _) x y = sameAlonzoTx proof x y
sameTx proof@(Conway _) x y = sameAlonzoTx proof x y
{-# NOINLINE sameTx #-}

-- ==========================
-- Comparing TxSeq for Sameness

ints :: [Int]
ints = [0 ..]

sameShelleyTxSeq ::
  ( Reflect era,
    Core.Tx era ~ ShelleyTx era,
    SafeToHash (Core.TxWits era)
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
  ( Reflect era,
    AlonzoEraTx era,
    SafeToHash (Core.TxWits era)
  ) =>
  Proof era ->
  AlonzoTxSeq era ->
  AlonzoTxSeq era ->
  [(String, Maybe PDoc)]
sameAlonzoTxSeq proof (AlonzoTxSeq ss1) (AlonzoTxSeq ss2) =
  sameWithDependency (zipWith3 f ints (toList ss1) (toList ss2))
  where
    f n t1 t2 = SomeM (show n) (sameTx proof) t1 t2

sameTxSeq :: Reflect era => Proof era -> Core.TxSeq era -> Core.TxSeq era -> [(String, Maybe PDoc)]
sameTxSeq proof@(Shelley _) x y = sameShelleyTxSeq proof x y
sameTxSeq proof@(Allegra _) x y = sameShelleyTxSeq proof x y
sameTxSeq proof@(Mary _) x y = sameShelleyTxSeq proof x y
sameTxSeq proof@(Alonzo _) x y = sameAlonzoTxSeq proof x y
sameTxSeq proof@(Babbage _) x y = sameAlonzoTxSeq proof x y
sameTxSeq proof@(Conway _) x y = sameAlonzoTxSeq proof x y
{-# NOINLINE sameTxSeq #-}
