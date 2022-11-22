{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic Consensus examples
--   This module is meant to replace a bunch of Consensus examples
--   distributed across many Era. By defining the Consensus examples
--   for every era once, by writing code that is parameterized by a (Proof era),
--   it should be easier to maintain. It is meant to be byte for byte exact.
module Test.Cardano.Ledger.Generic.Consensus where

import Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed as Seed
import Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Alonzo.Data
  ( AlonzoTxAuxData (..),
    Data (..),
    dataToBinaryData,
    hashData,
    mkAlonzoTxAuxData,
  )
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import qualified Cardano.Ledger.Alonzo.PParams as AlonzoPP
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), CostModels (..), ExUnits (..), Prices (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.AuxiliaryData
import qualified Cardano.Ledger.Babbage.PParams as BabbagePP
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), Datum (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import qualified Cardano.Ledger.Conway.PParams as ConwayPP
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto as CC
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.API hiding (RequireAllOf, RequireAnyOf, RequireMOf, RequireSignature)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import qualified Cardano.Ledger.Shelley.PParams as PParams (Update (..))
import qualified Cardano.Ledger.Shelley.PParams as ShelleyPP
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.ShelleyMA (ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AllegraTxAuxData (..))
import Cardano.Ledger.ShelleyMA.Era (MAClass)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import Cardano.Ledger.UTxO (makeWitnessesVKey)
import Cardano.Ledger.Val (inject)
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Cardano.Protocol.TPraos.Rules.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.EpochInfo
import qualified Data.ByteString as Strict
import Data.Coerce (coerce)
import Data.Default.Class
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Proxy
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Word (Word64, Word8)
import GHC.Records (HasField)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as Plutus
import qualified Test.Cardano.Ledger.Allegra.Examples.Consensus as Old (ledgerExamplesAllegra)
import Test.Cardano.Ledger.Alonzo.EraMapping ()
import qualified Test.Cardano.Ledger.Alonzo.Examples.Consensus as Old (ledgerExamplesAlonzo)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1)
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import qualified Test.Cardano.Ledger.Babbage.Examples.Consensus as Old (ledgerExamplesBabbage)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import qualified Test.Cardano.Ledger.Conway.Examples.Consensus as Old (ledgerExamplesConway)
import Test.Cardano.Ledger.Generic.Fields
  ( TxBodyField (..),
    TxField (..),
    TxOutField (..),
    WitnessesField (..),
  )
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters (merge, newTx, newTxBody, newTxOut, newWitnesses)
import qualified Test.Cardano.Ledger.Mary.Examples.Consensus as Old (ledgerExamplesMary)
import Test.Cardano.Ledger.Shelley.Examples.Consensus
  ( ShelleyLedgerExamples (..),
    ShelleyResultExamples (..),
  )
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as Old (ledgerExamplesShelley)
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE
import Test.Cardano.Ledger.Shelley.Generator.Core
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils hiding (mkVRFKeyPair)

-- ==================================================================

type KeyPairWits era = [KeyPair 'Witness (EraCrypto era)]

emptyPPUpdate :: Proof era -> Core.PParamsUpdate era
emptyPPUpdate (Shelley _) = ShelleyPP.emptyPParamsUpdate
emptyPPUpdate (Allegra _) = ShelleyPP.emptyPParamsUpdate
emptyPPUpdate (Mary _) = ShelleyPP.emptyPParamsUpdate
emptyPPUpdate (Alonzo _) = AlonzoPP.emptyPParamsUpdate
emptyPPUpdate (Babbage _) = BabbagePP.emptyPParamsUpdate
emptyPPUpdate (Conway _) = ConwayPP.emptyPParamsUpdate

emptyPP :: Proof era -> Core.PParams era
emptyPP (Shelley _) = ShelleyPP.emptyPParams
emptyPP (Allegra _) = ShelleyPP.emptyPParams
emptyPP (Mary _) = ShelleyPP.emptyPParams
emptyPP (Alonzo _) = AlonzoPP.emptyPParams
emptyPP (Babbage _) = BabbagePP.emptyPParams
emptyPP (Conway _) = ConwayPP.emptyPParams

-- ==================================================================
-- LedgerExamples
-- ==================================================================

mkWitnesses ::
  Reflect era =>
  Proof era ->
  Core.TxBody era ->
  KeyPairWits era ->
  Core.TxWits era
mkWitnesses proof txBody keyPairWits =
  genericWits proof [Just (AddrWits (makeWitnessesVKey (coerce (hashAnnotated txBody)) keyPairWits))]

defaultLedgerExamples ::
  forall era.
  ( Reflect era,
    Default (StashedAVVMAddresses era),
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  Proof era ->
  Core.Value era ->
  Core.TxBody era ->
  Core.TxAuxData era ->
  TranslationContext era ->
  ShelleyLedgerExamples era
defaultLedgerExamples proof value txBody auxData translationContext =
  ShelleyLedgerExamples
    { sleBlock = exampleLedgerBlock proof tx,
      sleHashHeader = exampleHashHeader proof,
      sleTx = tx,
      sleApplyTxError =
        ApplyTxError
          [ case proof of
              Shelley _ -> DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)
              Allegra _ -> DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)
              Mary _ -> DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)
              Alonzo _ -> DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)
              Babbage _ -> DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)
              Conway _ -> DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)
          ],
      sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100),
            Right (ScriptHashObj (mkScriptHash 1)),
            Right (KeyHashObj (mkKeyHash 2))
          ],
      sleResultExamples = resultExamples,
      sleNewEpochState =
        exampleNewEpochState
          proof
          value
          (emptyPP proof)
          ( case proof of
              Shelley _ -> (emptyPP proof) {_minUTxOValue = Coin 1}
              Allegra _ -> (emptyPP proof) {_minUTxOValue = Coin 1}
              Mary _ -> (emptyPP proof) {_minUTxOValue = Coin 1}
              Alonzo _ -> (emptyPP proof) {AlonzoPP._coinsPerUTxOWord = Coin 1}
              Babbage _ -> (emptyPP proof) {BabbagePP._coinsPerUTxOByte = Coin 1}
              Conway _ -> (emptyPP proof) {ConwayPP._coinsPerUTxOByte = Coin 1}
          ),
      sleChainDepState = exampleLedgerChainDepState 1,
      sleTranslationContext = translationContext
    }
  where
    tx = exampleTx proof txBody auxData

    resultExamples =
      ShelleyResultExamples
        { srePParams =
            ( case proof of
                Shelley _ -> def
                Allegra _ -> def
                Mary _ -> def
                Alonzo _ -> def
                Babbage _ -> def
                Conway _ -> def
            ),
          sreProposedPPUpdates = exampleProposedPParamsUpdates proof,
          srePoolDistr = examplePoolDistr,
          sreNonMyopicRewards = exampleNonMyopicRewards,
          sreShelleyGenesis = testShelleyGenesis
        }
{-# NOINLINE defaultLedgerExamples #-}

-- ============================================

mkKeyHash :: forall c discriminator. CC.Crypto c => Int -> KeyHash discriminator c
mkKeyHash = KeyHash . mkDummyHash @(ADDRHASH c)

mkScriptHash :: forall c. CC.Crypto c => Int -> ScriptHash c
mkScriptHash = ScriptHash . mkDummyHash @(ADDRHASH c)

mkDummySafeHash :: forall c a. CC.Crypto c => Proxy c -> Int -> SafeHash c a
mkDummySafeHash _ = unsafeMakeSafeHash . mkDummyHash @(HASH c)

exampleLedgerBlock ::
  forall era.
  Reflect era =>
  Proof era ->
  Core.Tx era ->
  Block (BHeader (EraCrypto era)) era
exampleLedgerBlock proof tx = specialize @EraSegWits proof (Block blockHeader blockBody)
  where
    keys :: AllIssuerKeys (EraCrypto era) 'StakePool
    keys = exampleKeys

    (_, (hotKey, _)) = head $ hot keys
    KeyPair vKeyCold _ = cold keys

    blockHeader :: BHeader (EraCrypto era)
    blockHeader = BHeader blockHeaderBody (signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: BHBody (EraCrypto era)
    blockHeaderBody =
      BHBody
        { bheaderBlockNo = BlockNo 3,
          bheaderSlotNo = SlotNo 9,
          bheaderPrev = BlockHash (HashHeader (mkDummyHash (2 :: Int))),
          bheaderVk = coerceKeyRole vKeyCold,
          bheaderVrfVk = snd $ vrf keys,
          bheaderEta = mkCertifiedVRF (mkBytes 0) (fst $ vrf keys),
          bheaderL = mkCertifiedVRF (mkBytes 1) (fst $ vrf keys),
          bsize = 2345,
          bhash = specialize @EraSegWits proof (hashTxSeq @era blockBody),
          bheaderOCert = mkOCert keys 0 (KESPeriod 0),
          bprotver = ProtVer (natVersion @2) 0
        }

    blockBody = specialize @EraSegWits proof (toTxSeq @era (StrictSeq.fromList [tx]))

    mkBytes :: Int -> Cardano.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash @Blake2b_256
{-# NOINLINE exampleLedgerBlock #-}

exampleKeys :: forall c r. CC.Crypto c => AllIssuerKeys c r
exampleKeys =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @c) 1)
    [(KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3))]
    (hashKey (vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

mkVRFKeyPair ::
  forall c.
  VRFAlgorithm (VRF c) =>
  Proxy c ->
  Word8 ->
  (Cardano.Ledger.Keys.SignKeyVRF c, Cardano.Ledger.Keys.VerKeyVRF c)
mkVRFKeyPair _ byte = (sk, VRF.deriveVerKeyVRF sk)
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (VRF.seedSizeVRF (Proxy @(VRF c))))
          byte

    sk = VRF.genKeyVRF seed

-- | @mkKeyPair'@ from @Test.Cardano.Ledger.Shelley.Utils@ doesn't work for real
-- crypto:
-- <https://github.com/input-output-hk/cardano-ledger/issues/1770>
mkDSIGNKeyPair ::
  forall c kd.
  DSIGNAlgorithm (DSIGN c) =>
  Word8 ->
  KeyPair kd c
mkDSIGNKeyPair byte = KeyPair (VKey $ DSIGN.deriveVerKeyDSIGN sk) sk
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (DSIGN.seedSizeDSIGN (Proxy @(DSIGN c))))
          byte

    sk = DSIGN.genKeyDSIGN seed

exampleHashHeader ::
  forall era.
  Reflect era =>
  Proof era ->
  HashHeader (EraCrypto era)
exampleHashHeader _proof = coerce $ mkDummyHash @(HASH (EraCrypto era)) (0 :: Int)

-- | Use this in the sreProposedPPUpdates field of ResultExample
--   Do not confuse this with exampleProposedPPUpdates which is used in exampleTxBody
exampleProposedPParamsUpdates ::
  Proof era ->
  ProposedPPUpdates era
exampleProposedPParamsUpdates (Shelley _) =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      (emptyPParamsUpdate {_keyDeposit = SJust (Coin 100)})
exampleProposedPParamsUpdates (Allegra _) =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      (emptyPParamsUpdate {_keyDeposit = SJust (Coin 100)})
exampleProposedPParamsUpdates proof@(Mary _) =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      ((emptyPPUpdate proof) {_keyDeposit = SJust (Coin 100)})
exampleProposedPParamsUpdates proof@(Alonzo _) =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      ((emptyPPUpdate proof) {AlonzoPP._collateralPercentage = SJust 150})
exampleProposedPParamsUpdates proof@(Babbage _) =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      ((emptyPPUpdate proof) {BabbagePP._collateralPercentage = SJust 150})
exampleProposedPParamsUpdates proof@(Conway _) =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      ((emptyPPUpdate proof) {BabbagePP._collateralPercentage = SJust 150})
{-# NOINLINE exampleProposedPParamsUpdates #-}

-- | Used to fill in the TxBody Update field
exampleProposedPPUpdates :: Proof era -> ProposedPPUpdates era
exampleProposedPPUpdates proof@(Shelley _) =
  ProposedPPUpdates $
    Map.singleton (mkKeyHash 1) ((emptyPPUpdate proof) {_maxBHSize = SJust 4000})
exampleProposedPPUpdates proof@(Allegra _) =
  ProposedPPUpdates $
    Map.singleton (mkKeyHash 1) ((emptyPPUpdate proof) {_maxBHSize = SJust 4000})
exampleProposedPPUpdates proof@(Mary _) =
  ProposedPPUpdates $
    Map.singleton (mkKeyHash 1) ((emptyPPUpdate proof) {_maxBHSize = SJust 4000})
exampleProposedPPUpdates proof@(Alonzo _) =
  ProposedPPUpdates $
    Map.singleton (mkKeyHash 1) ((emptyPPUpdate proof) {AlonzoPP._maxBHSize = SJust 4000})
exampleProposedPPUpdates proof@(Babbage _) =
  ProposedPPUpdates $
    Map.singleton (mkKeyHash 1) ((emptyPPUpdate proof) {BabbagePP._maxBHSize = SJust 4000})
exampleProposedPPUpdates proof@(Conway _) =
  ProposedPPUpdates $
    Map.singleton (mkKeyHash 1) ((emptyPPUpdate proof) {ConwayPP._maxBHSize = SJust 4000})
{-# NOINLINE exampleProposedPPUpdates #-}

exampleNonMyopicRewards ::
  forall c.
  CC.Crypto c =>
  Map
    (Either Coin (Credential 'Staking c))
    (Map (KeyHash 'StakePool c) Coin)
exampleNonMyopicRewards =
  Map.fromList
    [ (Left (Coin 100), Map.singleton (mkKeyHash 2) (Coin 3)),
      (Right (ScriptHashObj (mkScriptHash 1)), Map.empty),
      (Right (KeyHashObj (mkKeyHash 2)), Map.singleton (mkKeyHash 3) (Coin 9))
    ]

-- =====================================================================

-- | The EpochState has a field which is (PParams era). We need these
--     fields, a subset of the fields in PParams, in: startStep and createRUpd.
type UsesPP era =
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  )

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
  forall era.
  ( Reflect era,
    Default (StashedAVVMAddresses era),
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  Proof era ->
  Core.Value era ->
  Core.PParams era ->
  Core.PParams era ->
  NewEpochState era
exampleNewEpochState proof spendvalue ppp pp =
  NewEpochState
    { nesEL = EpochNo 0,
      nesBprev = BlocksMade (Map.singleton (mkKeyHash 1) 10),
      nesBcur = BlocksMade (Map.singleton (mkKeyHash 2) 3),
      nesEs = epochState,
      nesRu = SJust rewardUpdate,
      nesPd = examplePoolDistr,
      stashedAVVMAddresses = def
    }
  where
    epochState :: EpochState era
    epochState =
      EpochState
        { esAccountState =
            AccountState
              { asTreasury = Coin 10000,
                asReserves = Coin 1000
              },
          esSnapshots = emptySnapShots,
          esLState =
            LedgerState
              { lsUTxOState =
                  UTxOState
                    { utxosUtxo =
                        UTxO $
                          Map.fromList
                            [ ( TxIn (TxId (mkDummySafeHash Proxy 1)) minBound,
                                genericTxOut proof [Just (Address addr), Just (Amount spendvalue)]
                              )
                            ],
                      utxosDeposited = Coin 1000,
                      utxosFees = Coin 1,
                      utxosPpups = def,
                      utxosStakeDistr = mempty
                    },
                lsDPState = def
              },
          esPrevPp = ppp,
          esPp = pp,
          esNonMyopic = def
        }
      where
        addr :: Addr (EraCrypto era)
        addr =
          Addr
            Testnet
            (keyToCredential examplePayKey)
            (StakeRefBase (keyToCredential exampleStakeKey))

    rewardUpdate :: PulsingRewUpdate (EraCrypto era)
    rewardUpdate = case proof of
      Shelley _ -> step
      Allegra _ -> step
      Mary _ -> step
      Alonzo _ -> step
      Babbage _ -> step
      Conway _ -> step
    step :: UsesPP era => PulsingRewUpdate (EraCrypto era)
    step =
      startStep @era
        (EpochSize 432000)
        (BlocksMade (Map.singleton (mkKeyHash 1) 10))
        epochState
        (Coin 45)
        (activeSlotCoeff testGlobals)
        10

keyToCredential :: CC.Crypto c => KeyPair r c -> Credential r c
keyToCredential = KeyHashObj . hashKey . vKey

examplePayKey :: CC.Crypto c => KeyPair 'Payment c
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: CC.Crypto c => KeyPair 'Staking c
exampleStakeKey = mkDSIGNKeyPair 1

examplePoolDistr :: forall c. PraosCrypto c => PoolDistr c
examplePoolDistr =
  PoolDistr $
    Map.fromList
      [ ( mkKeyHash 1,
          IndividualPoolStake
            1
            (hashVerKeyVRF (snd (vrf (exampleKeys @c))))
        )
      ]

exampleLedgerChainDepState :: forall c. CC.Crypto c => Word64 -> ChainDepState c
exampleLedgerChainDepState seed =
  ChainDepState
    { csProtocol =
        PrtclState
          ( Map.fromList
              [ (mkKeyHash 1, 1),
                (mkKeyHash 2, 2)
              ]
          )
          (mkNonceFromNumber seed)
          (mkNonceFromNumber seed),
      csTickn =
        TicknState
          NeutralNonce
          (mkNonceFromNumber seed),
      csLabNonce =
        mkNonceFromNumber seed
    }

postAlonzoWits :: forall era. Reflect era => Proof era -> Core.TxBody era -> TxField era
postAlonzoWits proof txBody =
  WitnessesI
    [ AddrWits (makeWitnessesVKey (hashAnnotated txBody) [asWitness examplePayKey]),
      BootWits mempty,
      ScriptWits @era
        ( case proof of
            Alonzo _ ->
              Map.singleton
                (Core.hashScript @era $ alwaysSucceeds @era PlutusV1 3)
                (alwaysSucceeds @era PlutusV1 3)
            Babbage _ ->
              Map.singleton
                (Core.hashScript @era $ alwaysSucceeds @era PlutusV1 3)
                (alwaysSucceeds @era PlutusV1 3)
            Conway _ ->
              Map.singleton
                (Core.hashScript @era $ alwaysSucceeds @era PlutusV1 3)
                (alwaysSucceeds @era PlutusV1 3)
            other -> error ("Era " ++ show other ++ " is not a postAlonzo era.")
        ),
      DataWits
        ( TxDats @era $
            Map.singleton
              (Cardano.Ledger.Alonzo.Data.hashData @era datumExample)
              datumExample
        ),
      RdmrWits
        ( Redeemers @era $
            Map.singleton
              (RdmrPtr Tag.Spend 0)
              (redeemerExample @era, ExUnits 5000 5000)
        )
    ]
{-# NOINLINE postAlonzoWits #-}

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era.
  Reflect era =>
  Proof era ->
  Core.TxBody era ->
  Core.TxAuxData era ->
  Core.Tx era
exampleTx proof txBody auxData =
  genericTx
    proof
    [ -- Body
      Just (Body txBody),
      -- Witnesses
      case proof of
        Shelley _ -> Just (TxWits (mkWitnesses proof txBody keyPairWits))
        Allegra _ -> Just (TxWits (mkWitnesses proof txBody keyPairWits))
        Mary _ -> Just (TxWits (mkWitnesses proof txBody keyPairWits))
        Alonzo _ -> Just $ postAlonzoWits proof txBody
        Babbage _ -> Just $ postAlonzoWits proof txBody
        Conway _ -> Just $ postAlonzoWits proof txBody,
      -- Meta data
      case proof of
        Shelley _ -> Just (AuxData' [auxData])
        Allegra _ -> Just (AuxData' [auxData])
        Mary _ -> Just (AuxData' [auxData])
        Alonzo _ -> (Just . AuxData . SJust) exampleAlonzoTxAuxData
        Babbage _ -> (Just . AuxData . SJust) exampleAlonzoTxAuxData
        Conway _ -> (Just . AuxData . SJust) exampleAlonzoTxAuxData,
      -- Validity
      case proof of
        Shelley _ -> Nothing
        Allegra _ -> Nothing
        Mary _ -> Nothing
        Alonzo _ -> Just (Valid' True)
        Babbage _ -> Just (Valid' True)
        Conway _ -> Just (Valid' True)
    ]
  where
    keyPairWits :: KeyPairWits era
    keyPairWits =
      [ asWitness examplePayKey,
        asWitness exampleStakeKey,
        asWitness $ cold (exampleKeys @(EraCrypto era) @'StakePool)
      ]

exampleMint :: forall era. Proof era -> Maybe (MultiAsset (EraCrypto era))
exampleMint (Shelley _) = Nothing
exampleMint (Allegra _) = Just mempty
exampleMint (Mary _) = Just mempty
exampleMint (Alonzo _) = Just (multiAssetOf (exampleMultiAssetValue @(EraCrypto era) 3))
exampleMint (Babbage _) = Just (multiAssetOf (exampleMultiAssetValue @(EraCrypto era) 3))
exampleMint (Conway _) = Just (multiAssetOf (exampleMultiAssetValue @(EraCrypto era) 3))
{-# NOINLINE exampleMint #-}

exampleTxBody ::
  forall era.
  Reflect era =>
  Proof era ->
  Core.Value era ->
  Core.TxBody era
exampleTxBody proof spendval =
  genericTxBody
    proof
    [ Just (Inputs (exampleTxIns proof)),
      Just (Outputs' [exampleTxOut proof spendval]),
      Collateral <$> exampleCollateral proof,
      RefInputs <$> exampleRefInputs proof,
      (CollateralReturn . SJust) <$> exampleCollateralOutput proof,
      (TotalCol . SJust) <$> exampleTotalCollateral proof,
      Just (Certs exampleCerts),
      Just (Wdrls (exampleWithdrawals proof)),
      Just (Txfee (exampleFee proof)),
      exampleValidity proof,
      Just (Update' [PParams.Update (exampleProposedPPUpdates proof) (EpochNo 0)]),
      case proof of
        Shelley _ -> Just (AdHash' [auxiliaryDataHash])
        Allegra _ -> Just (AdHash' [auxiliaryDataHash])
        Mary _ -> Just (AdHash' [auxiliaryDataHash])
        Alonzo _ ->
          (Just . AdHash . SJust . AuxiliaryDataHash)
            (mkDummySafeHash (Proxy @(EraCrypto era)) 42)
        Babbage _ ->
          (Just . AdHash . SJust . AuxiliaryDataHash)
            (mkDummySafeHash (Proxy @(EraCrypto era)) 42)
        Conway _ ->
          (Just . AdHash . SJust . AuxiliaryDataHash)
            (mkDummySafeHash (Proxy @(EraCrypto era)) 42),
      Mint <$> exampleMint proof,
      case proof of
        Shelley _ -> Nothing
        Allegra _ -> Nothing
        Mary _ -> Nothing
        Alonzo _ -> Just (ReqSignerHashes (Set.singleton $ SLE.mkKeyHash 212))
        Babbage _ -> Just (ReqSignerHashes (Set.singleton $ SLE.mkKeyHash 212))
        Conway _ -> Just (ReqSignerHashes (Set.singleton $ SLE.mkKeyHash 212)),
      case proof of
        Shelley _ -> Nothing
        Allegra _ -> Nothing
        Mary _ -> Nothing
        Alonzo _ -> Just (WppHash' [mkDummySafeHash (Proxy @(EraCrypto era)) 42])
        Babbage _ -> Just (WppHash' [mkDummySafeHash (Proxy @(EraCrypto era)) 42])
        Conway _ -> Just (WppHash' [mkDummySafeHash (Proxy @(EraCrypto era)) 42]),
      case proof of
        Shelley _ -> Nothing
        Allegra _ -> Nothing
        Mary _ -> Nothing
        Alonzo _ -> Just (Txnetworkid (SJust Mainnet))
        Babbage _ -> Just (Txnetworkid (SJust Mainnet))
        Conway _ -> Just (Txnetworkid (SJust Mainnet))
    ]
  where
    -- Dummy hash to decouple from the auxiliaryData in 'exampleTx'.
    auxiliaryDataHash :: AuxiliaryDataHash (EraCrypto era)
    auxiliaryDataHash =
      AuxiliaryDataHash $ mkDummySafeHash (Proxy @(EraCrypto era)) 30
{-# NOINLINE exampleTxBody #-}

exampleValidity :: Proof era -> Maybe (TxBodyField era)
exampleValidity proof =
  case proof of
    Shelley _ -> Just (TTL (SlotNo 10))
    Allegra _ -> Just (Vldt (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))))
    Mary _ -> Just (Vldt (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))))
    Alonzo _ -> Just (Vldt (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))))
    Babbage _ -> Just (Vldt (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))))
    Conway _ -> Just (Vldt (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))))

exampleFee :: Proof era -> Coin
exampleFee proof =
  case proof of
    Shelley _ -> Coin 3
    Allegra _ -> Coin 3
    Mary _ -> Coin 3
    Alonzo _ -> Coin 999
    Babbage _ -> Coin 999
    Conway _ -> Coin 999

exampleTxIns :: Era era => Proof era -> Set (TxIn (EraCrypto era))
exampleTxIns proof = Set.fromList $
  case proof of
    Shelley _ -> [TxIn (TxId (mkDummySafeHash Proxy 1)) minBound]
    Allegra _ -> [TxIn (TxId (mkDummySafeHash Proxy 1)) minBound]
    Mary _ -> [TxIn (TxId (mkDummySafeHash Proxy 1)) minBound]
    Alonzo _ -> [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 0]
    Babbage _ -> [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 0]
    Conway _ -> [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 0]

exampleCollateral :: Era era => Proof era -> Maybe (Set (TxIn (EraCrypto era)))
exampleCollateral proof =
  case proof of
    Shelley _ -> Nothing
    Allegra _ -> Nothing
    Mary _ -> Nothing
    Alonzo _ -> Just (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 2)) 1])
    Babbage _ -> Just (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 2)) 1])
    Conway _ -> Just (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 2)) 1])

exampleCollateralOutput :: Era era => Proof era -> Maybe (Core.TxOut era)
exampleCollateralOutput proof =
  case proof of
    Shelley _ -> Nothing
    Allegra _ -> Nothing
    Mary _ -> Nothing
    Alonzo _ -> Nothing
    Babbage _ ->
      Just $
        BabbageTxOut
          (mkAddr (examplePayKey, exampleStakeKey))
          (MaryValue 8675309 mempty)
          NoDatum
          SNothing
    Conway _ ->
      Just $
        BabbageTxOut
          (mkAddr (examplePayKey, exampleStakeKey))
          (MaryValue 8675309 mempty)
          NoDatum
          SNothing

exampleTotalCollateral :: Proof era -> Maybe Coin
exampleTotalCollateral proof =
  case proof of
    Shelley _ -> Nothing
    Allegra _ -> Nothing
    Mary _ -> Nothing
    Alonzo _ -> Nothing
    Babbage _ -> Just (Coin 8675309)
    Conway _ -> Just (Coin 8675309)

exampleRefInputs :: Era era => Proof era -> Maybe (Set (TxIn (EraCrypto era)))
exampleRefInputs proof =
  case proof of
    Shelley _ -> Nothing
    Allegra _ -> Nothing
    Mary _ -> Nothing
    Alonzo _ -> Nothing
    Babbage _ -> Just (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 3])
    Conway _ -> Just (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 3])

exampleTxOut :: Proof era -> Core.Value era -> Core.TxOut era
exampleTxOut proof spendval =
  case proof of
    Shelley _ -> ShelleyTxOut (mkAddr (examplePayKey, exampleStakeKey)) spendval
    Allegra _ -> ShelleyTxOut (mkAddr (examplePayKey, exampleStakeKey)) spendval
    Mary _ ->
      ShelleyTxOut
        (mkAddr (examplePayKey, exampleStakeKey))
        spendval
    Alonzo _ ->
      AlonzoTxOut
        (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
        spendval
        (SJust $ SLE.mkDummySafeHash Proxy 1)
    Babbage _ ->
      BabbageTxOut
        (mkAddr (examplePayKey, exampleStakeKey))
        (exampleMultiAssetValue 2)
        (Datum $ dataToBinaryData datumExample) -- inline datum
        (SJust $ alwaysSucceeds PlutusV2 3) -- reference script
    Conway _ ->
      BabbageTxOut
        (mkAddr (examplePayKey, exampleStakeKey))
        (exampleMultiAssetValue 2)
        (Datum $ dataToBinaryData datumExample) -- inline datum
        (SJust $ alwaysSucceeds PlutusV2 3) -- reference script

exampleCerts :: CC.Crypto c => StrictSeq (DCert c)
exampleCerts =
  StrictSeq.fromList
    [ DCertDeleg (RegKey (keyToCredential exampleStakeKey)),
      DCertPool (RegPool examplePoolParams),
      DCertMir $
        MIRCert ReservesMIR $
          StakeAddressesMIR $
            Map.fromList
              [ (keyToCredential (mkDSIGNKeyPair 2), DeltaCoin 110)
              ]
    ]

examplePoolParams :: forall c. CC.Crypto c => PoolParams c
examplePoolParams =
  PoolParams
    { ppId = hashKey $ vKey $ cold poolKeys,
      ppVrf = hashVerKeyVRF $ snd $ vrf poolKeys,
      ppPledge = Coin 1,
      ppCost = Coin 5,
      ppMargin = unsafeBoundRational 0.1,
      ppRewardAcnt = RewardAcnt Testnet (keyToCredential exampleStakeKey),
      ppOwners = Set.singleton $ hashKey $ vKey exampleStakeKey,
      ppRelays = StrictSeq.empty,
      ppMetadata =
        SJust $
          PoolMetadata
            { pmUrl = fromJust $ textToUrl "consensus.pool",
              pmHash = "{}"
            }
    }
  where
    poolKeys = exampleKeys @c @'StakePool

exampleWithdrawals :: Era era => Proof era -> Wdrl (EraCrypto era)
exampleWithdrawals proof =
  case proof of
    Shelley _ -> Wdrl $ Map.fromList [(ppRewardAcnt examplePoolParams, Coin 100)]
    Allegra _ -> Wdrl $ Map.fromList [(ppRewardAcnt examplePoolParams, Coin 100)]
    Mary _ -> Wdrl $ Map.fromList [(ppRewardAcnt examplePoolParams, Coin 100)]
    Alonzo _ ->
      Wdrl $
        Map.singleton
          (RewardAcnt Testnet (keyToCredential exampleStakeKey))
          (Coin 100)
    Babbage _ ->
      Wdrl $
        Map.singleton
          (RewardAcnt Testnet (keyToCredential exampleStakeKey))
          (Coin 100)
    Conway _ ->
      Wdrl $
        Map.singleton
          (RewardAcnt Testnet (keyToCredential exampleStakeKey))
          (Coin 100)

-- | These are dummy values.
testShelleyGenesis :: ShelleyGenesis era
testShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = UTCTime (fromGregorian 2020 5 14) 0,
      sgNetworkMagic = 0,
      sgNetworkId = Testnet,
      -- Chosen to match activeSlotCoeff
      sgActiveSlotsCoeff = unsafeBoundRational 0.9,
      sgSecurityParam = securityParameter testGlobals,
      sgEpochLength = runIdentity $ epochInfoSize testEpochInfo 0,
      sgSlotsPerKESPeriod = slotsPerKESPeriod testGlobals,
      sgMaxKESEvolutions = maxKESEvo testGlobals,
      -- Not important
      sgSlotLength = secondsToNominalDiffTime 2,
      sgUpdateQuorum = quorum testGlobals,
      sgMaxLovelaceSupply = maxLovelaceSupply testGlobals,
      sgProtocolParams = emptyPParams,
      sgGenDelegs = Map.empty,
      sgInitialFunds = mempty,
      sgStaking = emptyGenesisStaking
    }

testEpochInfo :: EpochInfo Identity
testEpochInfo = epochInfoPure testGlobals

-- ================================================================================

-- | Build a Tx from a list of maybe fields. The maybe is usfull because sometimes we
--   only want to add a field in a particular era, so we might say something like this
--
-- @
-- genericTx proof
--  [Just field1
--  , if (Some proof) >= Mary Mock then Just field2 else Nothing
--  , Just field3
--  ]
-- @
genericTx :: Proof era -> [Maybe (TxField era)] -> Core.Tx era
genericTx proof xs = newTx proof (foldl' accum [] xs)
  where
    accum ans Nothing = ans
    accum ans (Just field) = field : ans

-- | Build a Tx from a list of maybe fields. The maybe is usefull because sometimes we
--   only want to add a field in a particular era, so we might say something like this
--
-- @
-- genericTxBody proof
--  [Just field1
--  , if (Some proof) >= Mary Mock then Just field2 else Nothing
--  , Just field3
--  ]
-- @
genericTxBody :: Era era => Proof era -> [Maybe (TxBodyField era)] -> Core.TxBody era
genericTxBody proof xs = specialize @Core.EraTxBody proof (newTxBody proof (catMaybes xs))

genericTxOut :: Era era => Proof era -> [Maybe (TxOutField era)] -> Core.TxOut era
genericTxOut proof xs = newTxOut proof (catMaybes xs)

genericWits :: Era era => Proof era -> [Maybe (WitnessesField era)] -> Core.TxWits era
genericWits proof xs = newWitnesses merge proof (catMaybes xs)

-- =========================================================
-- Individual examples for each Era
-- =========================================================

-- | ShelleyLedgerExamples for Shelley era
ledgerExamplesShelley :: ShelleyLedgerExamples (ShelleyEra StandardCrypto)
ledgerExamplesShelley =
  defaultLedgerExamples
    (Shelley Standard)
    exampleCoin
    (exampleTxBody (Shelley Standard) (inject (Coin 100000)))
    exampleShelleyTxAuxData
    ()

exampleCoin :: Coin
exampleCoin = Coin 10

exampleAuxDataMap :: Map Word64 Metadatum
exampleAuxDataMap =
  Map.fromList
    [ (1, S "string"),
      (2, B "bytes"),
      (3, List [I 1, I 2]),
      (4, Map [(I 3, B "b")])
    ]

exampleShelleyTxAuxData :: Core.TxAuxData (ShelleyEra StandardCrypto)
exampleShelleyTxAuxData = ShelleyTxAuxData SLE.exampleAuxDataMap

-- ======================

ledgerExamplesAllegra :: ShelleyLedgerExamples (AllegraEra StandardCrypto)
ledgerExamplesAllegra =
  defaultLedgerExamples
    (Allegra Standard)
    exampleCoin
    (exampleTxBody (Allegra Standard) (inject exampleCoin))
    exampleAllegraTxAuxData
    ()

exampleAllegraTxAuxData :: MAClass ma c => AllegraTxAuxData (ShelleyMAEra ma c)
exampleAllegraTxAuxData =
  AllegraTxAuxData
    exampleAuxDataMap
    (StrictSeq.fromList [exampleScriptMA])

exampleScriptMA :: MAClass ma c => Core.Script (ShelleyMAEra ma c)
exampleScriptMA =
  RequireMOf 2 $
    StrictSeq.fromList
      [ RequireAllOf $
          StrictSeq.fromList
            [ RequireTimeStart (SlotNo 0),
              RequireTimeExpire (SlotNo 9)
            ],
        RequireAnyOf $
          StrictSeq.fromList
            [ RequireSignature (mkKeyHash 0),
              RequireSignature (mkKeyHash 1)
            ],
        RequireSignature (mkKeyHash 100)
      ]

-- ==============================================

ledgerExamplesMary :: ShelleyLedgerExamples (MaryEra StandardCrypto)
ledgerExamplesMary =
  defaultLedgerExamples
    (Mary Standard)
    (exampleMultiAssetValue 1)
    ( exampleTxBody @(MaryEra StandardCrypto)
        (Mary Standard)
        (exampleMultiAssetValue @CC.StandardCrypto 1)
    )
    exampleAllegraTxAuxData
    ()

exampleMultiAssetValue ::
  forall c.
  CC.Crypto c =>
  Int ->
  MaryValue c
exampleMultiAssetValue x =
  MaryValue 100 $ MultiAsset (Map.singleton policyId $ (Map.singleton couttsCoin 1000))
  where
    policyId :: PolicyID c
    policyId = PolicyID $ mkScriptHash x

    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"

multiAssetOf :: forall c. MaryValue c -> MultiAsset c
multiAssetOf (MaryValue _ ma) = ma

-- ============================================================

exampleAlonzoTxAuxData :: Era era => AlonzoTxAuxData era
exampleAlonzoTxAuxData =
  mkAlonzoTxAuxData
    SLE.exampleAuxDataMap
    [alwaysFails PlutusV1 2, TimelockScript $ RequireAllOf mempty]

ledgerExamplesAlonzo :: ShelleyLedgerExamples (AlonzoEra StandardCrypto)
ledgerExamplesAlonzo =
  defaultLedgerExamples
    (Alonzo Standard)
    (exampleMultiAssetValue 1)
    ( exampleTxBody
        (Alonzo Standard)
        (exampleMultiAssetValue @StandardCrypto 2)
    )
    exampleAlonzoTxAuxData
    exampleAlonzoGenesis

datumExample :: Era era => Data era
datumExample = Data (Plutus.I 191)

redeemerExample :: Era era => Data era
redeemerExample = Data (Plutus.I 919)

exampleAlonzoGenesis :: AlonzoGenesis
exampleAlonzoGenesis =
  AlonzoGenesis
    { coinsPerUTxOWord = Coin 1,
      costmdls = CostModels $ Map.fromList [(PlutusV1, testingCostModelV1)],
      prices = Prices (boundRational' 90) (boundRational' 91),
      maxTxExUnits = ExUnits 123 123,
      maxBlockExUnits = ExUnits 223 223,
      maxValSize = 1234,
      collateralPercentage = 20,
      maxCollateralInputs = 30
    }
  where
    boundRational' :: HasCallStack => Rational -> NonNegativeInterval
    boundRational' x = case boundRational x of
      Nothing -> error $ "Expected non-negative value but got: " <> show x
      Just x' -> x'

-- ============================================================

ledgerExamplesBabbage :: ShelleyLedgerExamples (BabbageEra StandardCrypto)
ledgerExamplesBabbage =
  defaultLedgerExamples
    (Babbage Standard)
    (exampleMultiAssetValue 1)
    ( exampleTxBody
        (Babbage Standard)
        (exampleMultiAssetValue @StandardCrypto 2)
    )
    exampleAlonzoTxAuxData
    exampleAlonzoGenesis

-- ============================================================

exampleConwayGenesis :: ConwayGenesis crypto
exampleConwayGenesis = ConwayGenesis (GenDelegs Map.empty)

ledgerExamplesConway :: ShelleyLedgerExamples (ConwayEra StandardCrypto)
ledgerExamplesConway =
  defaultLedgerExamples
    (Conway Standard)
    (exampleMultiAssetValue 1)
    ( exampleTxBody
        (Conway Standard)
        (exampleMultiAssetValue @StandardCrypto 2)
    )
    exampleAlonzoTxAuxData
    exampleConwayGenesis

-- ==============================================
-- Two functions that given a Proof, return the
-- generic examples, and the specific ones (old style)

ledgerExamples ::
  (EraCrypto era ~ StandardCrypto) =>
  Proof era ->
  ShelleyLedgerExamples era
ledgerExamples proof =
  case proof of
    Shelley _ -> ledgerExamplesShelley
    Allegra _ -> ledgerExamplesAllegra
    Mary _ -> ledgerExamplesMary
    Alonzo _ -> ledgerExamplesAlonzo
    Babbage _ -> ledgerExamplesBabbage
    Conway _ -> ledgerExamplesConway

oldLedgerExamples ::
  (EraCrypto era ~ StandardCrypto) =>
  Proof era ->
  ShelleyLedgerExamples era
oldLedgerExamples proof =
  case proof of
    Shelley _ -> Old.ledgerExamplesShelley
    Allegra _ -> Old.ledgerExamplesAllegra
    Mary _ -> Old.ledgerExamplesMary
    Alonzo _ -> Old.ledgerExamplesAlonzo
    Babbage _ -> Old.ledgerExamplesBabbage
    Conway _ -> Old.ledgerExamplesConway
