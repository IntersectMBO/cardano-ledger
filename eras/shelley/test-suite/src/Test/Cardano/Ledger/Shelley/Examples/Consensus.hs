{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Examples.Consensus where

import Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed as Seed
import Cardano.Crypto.VRF as VRF
import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto as CC
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Era
import Cardano.Ledger.Keys
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.TxWits
import Cardano.Ledger.UTxO
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Cardano.Protocol.TPraos.Rules.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.EpochInfo
import qualified Data.ByteString as Strict
import Data.Coerce (coerce)
import Data.Default.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Word (Word64, Word8)
import GHC.Records (HasField)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Shelley.Generator.Core
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils hiding (mkVRFKeyPair)

type KeyPairWits era = [KeyPair 'Witness (EraCrypto era)]

{-------------------------------------------------------------------------------
  ShelleyLedgerExamples
-------------------------------------------------------------------------------}

data ShelleyResultExamples era = ShelleyResultExamples
  { srePParams :: Core.PParams era,
    sreProposedPPUpdates :: ProposedPPUpdates era,
    srePoolDistr :: PoolDistr (EraCrypto era),
    sreNonMyopicRewards ::
      Map
        (Either Coin (Credential 'Staking (EraCrypto era)))
        (Map (KeyHash 'StakePool (EraCrypto era)) Coin),
    sreShelleyGenesis :: ShelleyGenesis era
  }

deriving instance
  ( Eq (Core.PParams era),
    Eq (Core.PParamsUpdate era)
  ) =>
  Eq (ShelleyResultExamples era)

data ShelleyLedgerExamples era = ShelleyLedgerExamples
  { sleBlock :: Block (BHeader (EraCrypto era)) era,
    sleHashHeader :: HashHeader (EraCrypto era),
    sleTx :: Core.Tx era,
    sleApplyTxError :: ApplyTxError era,
    sleRewardsCredentials :: Set (Either Coin (Credential 'Staking (EraCrypto era))),
    sleResultExamples :: ShelleyResultExamples era,
    sleNewEpochState :: NewEpochState era,
    sleChainDepState :: ChainDepState (EraCrypto era),
    sleTranslationContext :: TranslationContext era
  }

deriving instance
  ( Eq (Core.PParams era),
    Eq (Core.PParamsUpdate era),
    Eq (TxSeq era),
    Eq (Core.Tx era),
    Eq (PredicateFailure (Core.EraRule "LEDGER" era)),
    Eq (Core.TxOut era),
    Eq (State (Core.EraRule "PPUP" era)),
    Eq (StashedAVVMAddresses era),
    Eq (TranslationContext era),
    Era era
  ) =>
  Eq (ShelleyLedgerExamples era)

{-------------------------------------------------------------------------------
  Default constructor
-------------------------------------------------------------------------------}

type ShelleyBasedEra' era =
  ( Default (State (Core.EraRule "PPUP" era)),
    PraosCrypto (EraCrypto era)
  )

defaultShelleyLedgerExamples ::
  forall era.
  ( ShelleyBasedEra' era,
    EraSegWits era,
    PredicateFailure (Core.EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era,
    PredicateFailure (Core.EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era,
    Core.PParams era ~ ShelleyPParams era,
    Core.PParamsUpdate era ~ ShelleyPParamsUpdate era,
    Default (StashedAVVMAddresses era)
  ) =>
  (Core.TxBody era -> KeyPairWits era -> Core.TxWits era) ->
  (ShelleyTx era -> Core.Tx era) ->
  Core.Value era ->
  Core.TxBody era ->
  Core.TxAuxData era ->
  TranslationContext era ->
  ShelleyLedgerExamples era
defaultShelleyLedgerExamples mkWitnesses mkAlonzoTx value txBody auxData translationContext =
  ShelleyLedgerExamples
    { sleBlock = exampleShelleyLedgerBlock (mkAlonzoTx tx),
      sleHashHeader = exampleHashHeader (Proxy @era),
      sleTx = mkAlonzoTx tx,
      sleApplyTxError =
        ApplyTxError
          [DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)],
      sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100),
            Right (ScriptHashObj (mkScriptHash 1)),
            Right (KeyHashObj (mkKeyHash 2))
          ],
      sleResultExamples = resultExamples,
      sleNewEpochState =
        exampleNewEpochState
          value
          emptyPParams
          (emptyPParams {_minUTxOValue = Coin 1}),
      sleChainDepState = exampleLedgerChainDepState 1,
      sleTranslationContext = translationContext
    }
  where
    tx = exampleTx mkWitnesses txBody auxData

    resultExamples =
      ShelleyResultExamples
        { srePParams = def,
          sreProposedPPUpdates = exampleProposedPParamsUpdates,
          srePoolDistr = examplePoolDistr,
          sreNonMyopicRewards = exampleNonMyopicRewards,
          sreShelleyGenesis = testShelleyGenesis
        }

{-------------------------------------------------------------------------------
  Helper constructors
-------------------------------------------------------------------------------}

exampleShelleyLedgerBlock ::
  forall era.
  (EraSegWits era, ShelleyBasedEra' era) =>
  Core.Tx era ->
  Block (BHeader (EraCrypto era)) era
exampleShelleyLedgerBlock tx = Block blockHeader blockBody
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
          bhash = hashTxSeq @era blockBody,
          bheaderOCert = mkOCert keys 0 (KESPeriod 0),
          bprotver = ProtVer (natVersion @2) 0
        }

    blockBody = toTxSeq @era (StrictSeq.fromList [tx])

    mkBytes :: Int -> Cardano.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash @Blake2b_256

exampleHashHeader ::
  forall era.
  ShelleyBasedEra' era =>
  Proxy era ->
  HashHeader (EraCrypto era)
exampleHashHeader _ = coerce $ mkDummyHash @(HASH (EraCrypto era)) (0 :: Int)

mkKeyHash :: forall c discriminator. CC.Crypto c => Int -> KeyHash discriminator c
mkKeyHash = KeyHash . mkDummyHash @(ADDRHASH c)

mkScriptHash :: forall c. CC.Crypto c => Int -> ScriptHash c
mkScriptHash = ScriptHash . mkDummyHash @(ADDRHASH c)

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era.
  Core.EraTx era =>
  (Core.TxBody era -> KeyPairWits era -> Core.TxWits era) ->
  Core.TxBody era ->
  Core.TxAuxData era ->
  ShelleyTx era
exampleTx mkWitnesses txBody auxData =
  ShelleyTx txBody (mkWitnesses txBody keyPairWits) (SJust auxData)
  where
    keyPairWits :: KeyPairWits era
    keyPairWits =
      [ asWitness examplePayKey,
        asWitness exampleStakeKey,
        asWitness $ cold (exampleKeys @(EraCrypto era) @'StakePool)
      ]

exampleProposedPParamsUpdates ::
  ( ShelleyBasedEra' era,
    Core.PParamsUpdate era ~ ShelleyPParamsUpdate era
  ) =>
  ProposedPPUpdates era
exampleProposedPParamsUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      (emptyPParamsUpdate {_keyDeposit = SJust (Coin 100)})

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

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
  forall era.
  ( ShelleyBasedEra' era,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval,
    Default (StashedAVVMAddresses era),
    Core.EraTxOut era
  ) =>
  Core.Value era ->
  Core.PParams era ->
  Core.PParams era ->
  NewEpochState era
exampleNewEpochState value ppp pp =
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
                                Core.mkBasicTxOut addr value
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
    rewardUpdate =
      startStep @era
        (EpochSize 432000)
        (BlocksMade (Map.singleton (mkKeyHash 1) 10))
        epochState
        (Coin 45)
        (activeSlotCoeff testGlobals)
        10

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

testEpochInfo :: EpochInfo Identity
testEpochInfo = epochInfoPure testGlobals

mkDummySafeHash :: forall c a. CC.Crypto c => Proxy c -> Int -> SafeHash c a
mkDummySafeHash _ =
  unsafeMakeSafeHash
    . mkDummyHash @(HASH c)

{-------------------------------------------------------------------------------
  Shelley era specific functions
-------------------------------------------------------------------------------}

-- | ShelleyLedgerExamples for Shelley era
ledgerExamplesShelley :: ShelleyLedgerExamples Shelley
ledgerExamplesShelley =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @Shelley))
    id
    exampleCoin
    exampleTxBodyShelley
    exampleAuxiliaryDataShelley
    ()

mkWitnessesPreAlonzo ::
  ( Core.EraTx era,
    DSIGN.Signable
      (DSIGN (EraCrypto era))
      (Hash.Hash (HASH (EraCrypto era)) Core.EraIndependentTxBody)
  ) =>
  Proxy era ->
  Core.TxBody era ->
  KeyPairWits era ->
  ShelleyTxWits era
mkWitnessesPreAlonzo _ txBody keyPairWits =
  mempty
    { addrWits =
        makeWitnessesVKey (coerce (hashAnnotated txBody)) keyPairWits
    }

exampleCoin :: Coin
exampleCoin = Coin 10

exampleTxBodyShelley :: ShelleyTxBody Shelley
exampleTxBodyShelley =
  ShelleyTxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ ShelleyTxOut (mkAddr (examplePayKey, exampleStakeKey)) (Coin 100000)
        ]
    )
    exampleCerts
    exampleWithdrawals
    (Coin 3)
    (SlotNo 10)
    (SJust (Update exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
  where
    -- Dummy hash to decouple from the auxiliaryData in 'exampleTx'.
    auxiliaryDataHash :: AuxiliaryDataHash StandardCrypto
    auxiliaryDataHash =
      AuxiliaryDataHash $ mkDummySafeHash (Proxy @StandardCrypto) 30

exampleAuxDataMap :: Map Word64 Metadatum
exampleAuxDataMap =
  Map.fromList
    [ (1, S "string"),
      (2, B "bytes"),
      (3, List [I 1, I 2]),
      (4, Map [(I 3, B "b")])
    ]

exampleAuxiliaryDataShelley :: Core.TxAuxData Shelley
exampleAuxiliaryDataShelley = ShelleyTxAuxData exampleAuxDataMap

exampleTxIns :: CC.Crypto c => Set (TxIn c)
exampleTxIns =
  Set.fromList
    [ TxIn (TxId (mkDummySafeHash Proxy 1)) minBound
    ]

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

exampleWithdrawals :: CC.Crypto c => Wdrl c
exampleWithdrawals =
  Wdrl $
    Map.fromList
      [ (ppRewardAcnt examplePoolParams, Coin 100)
      ]

exampleProposedPPUpdates ::
  ( Core.PParamsUpdate era ~ ShelleyPParamsUpdate era,
    ShelleyBasedEra' era
  ) =>
  ProposedPPUpdates era
exampleProposedPPUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 1)
      (emptyPParamsUpdate {_maxBHSize = SJust 4000})

examplePayKey :: CC.Crypto c => KeyPair 'Payment c
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: CC.Crypto c => KeyPair 'Staking c
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys :: forall c r. CC.Crypto c => AllIssuerKeys c r
exampleKeys =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @c) 1)
    [(KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3))]
    (hashKey (vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

keyToCredential :: CC.Crypto c => KeyPair r c -> Credential r c
keyToCredential = KeyHashObj . hashKey . vKey

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
