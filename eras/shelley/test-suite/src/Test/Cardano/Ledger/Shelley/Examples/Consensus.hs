{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Examples.Consensus where

import Cardano.Binary
import Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed as Seed
import Cardano.Crypto.VRF as VRF
import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Era
import qualified Cardano.Ledger.Era as Era (Crypto)
import Cardano.Ledger.Keys
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Serialization
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Constraints
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Rules.Delegs
import Cardano.Ledger.Shelley.Rules.Ledger
import Cardano.Ledger.Shelley.Tx
import Cardano.Ledger.Shelley.UTxO
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Cardano.Protocol.TPraos.Rules.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.Block
import Cardano.Slotting.EpochInfo
import Cardano.Slotting.Slot
import qualified Data.ByteString as Strict
import Data.Coerce (coerce)
import qualified Data.Compact.SplitMap as SplitMap
import Data.Default.Class
import Data.Functor.Identity (Identity (..))
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
import Test.Cardano.Ledger.Shelley.Generator.Core
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils hiding (mkVRFKeyPair)

type KeyPairWits era = [KeyPair 'Witness (Cardano.Ledger.Era.Crypto era)]

{-------------------------------------------------------------------------------
  ShelleyLedgerExamples
-------------------------------------------------------------------------------}

data ShelleyResultExamples era = ShelleyResultExamples
  { srePParams :: Core.PParams era,
    sreProposedPPUpdates :: ProposedPPUpdates era,
    srePoolDistr :: PoolDistr (Cardano.Ledger.Era.Crypto era),
    sreNonMyopicRewards ::
      Map
        (Either Coin (Credential 'Staking (Cardano.Ledger.Era.Crypto era)))
        (Map (KeyHash 'StakePool (Cardano.Ledger.Era.Crypto era)) Coin),
    sreShelleyGenesis :: ShelleyGenesis era
  }

data ShelleyLedgerExamples era = ShelleyLedgerExamples
  { sleBlock :: Block (BHeader (Era.Crypto era)) era,
    sleHashHeader :: HashHeader (Cardano.Ledger.Era.Crypto era),
    sleTx :: Core.Tx era,
    sleApplyTxError :: ApplyTxError era,
    sleRewardsCredentials :: Set (Either Coin (Credential 'Staking (Cardano.Ledger.Era.Crypto era))),
    sleResultExamples :: ShelleyResultExamples era,
    sleNewEpochState :: NewEpochState era,
    sleChainDepState :: ChainDepState (Cardano.Ledger.Era.Crypto era),
    sleTranslationContext :: TranslationContext era
  }

{-------------------------------------------------------------------------------
  Default constructor
-------------------------------------------------------------------------------}

type ShelleyBasedEra' era =
  ( ShelleyBasedEra era,
    ToCBORGroup (TxSeq era),
    ToCBOR (Core.Witnesses era),
    Default (State (Core.EraRule "PPUP" era)),
    PraosCrypto (Cardano.Ledger.Era.Crypto era)
  )

defaultShelleyLedgerExamples ::
  forall era.
  ( ShelleyBasedEra' era,
    PredicateFailure (Core.EraRule "DELEGS" era)
      ~ DelegsPredicateFailure era,
    Core.PParams era ~ Cardano.Ledger.Shelley.PParams.PParams era,
    Core.PParamsDelta era ~ PParams' StrictMaybe era
  ) =>
  (Core.TxBody era -> KeyPairWits era -> Core.Witnesses era) ->
  (Tx era -> Core.Tx era) ->
  Core.Value era ->
  Core.TxBody era ->
  Core.AuxiliaryData era ->
  TranslationContext era ->
  ShelleyLedgerExamples era
defaultShelleyLedgerExamples mkWitnesses mkValidatedTx value txBody auxData translationContext =
  ShelleyLedgerExamples
    { sleBlock = exampleShelleyLedgerBlock (mkValidatedTx tx),
      sleHashHeader = exampleHashHeader (Proxy @era),
      sleTx = mkValidatedTx tx,
      sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @era (mkKeyHash 1),
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
  ShelleyBasedEra' era =>
  Core.Tx era ->
  Block (BHeader (Era.Crypto era)) era
exampleShelleyLedgerBlock tx = Block blockHeader blockBody
  where
    keys :: AllIssuerKeys (Cardano.Ledger.Era.Crypto era) 'StakePool
    keys = exampleKeys

    (_, (hotKey, _)) = head $ hot keys
    KeyPair vKeyCold _ = cold keys

    blockHeader :: BHeader (Cardano.Ledger.Era.Crypto era)
    blockHeader = BHeader blockHeaderBody (signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: BHBody (Cardano.Ledger.Era.Crypto era)
    blockHeaderBody =
      BHBody
        { bheaderBlockNo = BlockNo 3,
          bheaderSlotNo = SlotNo 9,
          bheaderPrev = BlockHash (HashHeader (mkDummyHash Proxy 2)),
          bheaderVk = coerceKeyRole vKeyCold,
          bheaderVrfVk = snd $ vrf keys,
          bheaderEta = mkCertifiedVRF (mkBytes 0) (fst $ vrf keys),
          bheaderL = mkCertifiedVRF (mkBytes 1) (fst $ vrf keys),
          bsize = 2345,
          bhash = hashTxSeq @era blockBody,
          bheaderOCert = mkOCert keys 0 (KESPeriod 0),
          bprotver = ProtVer 2 0
        }

    blockBody = toTxSeq @era (StrictSeq.fromList [tx])

    mkBytes :: Int -> Cardano.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash (Proxy @Blake2b_256)

exampleHashHeader ::
  forall era.
  ShelleyBasedEra' era =>
  Proxy era ->
  HashHeader (Cardano.Ledger.Era.Crypto era)
exampleHashHeader _ = coerce $ mkDummyHash (Proxy @(HASH (Cardano.Ledger.Era.Crypto era))) 0

mkKeyHash :: forall c discriminator. Cardano.Ledger.Crypto.Crypto c => Int -> KeyHash discriminator c
mkKeyHash = KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

mkScriptHash :: forall c. Cardano.Ledger.Crypto.Crypto c => Int -> ScriptHash c
mkScriptHash = ScriptHash . mkDummyHash (Proxy @(ADDRHASH c))

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era.
  ShelleyBasedEra' era =>
  (Core.TxBody era -> KeyPairWits era -> Core.Witnesses era) ->
  Core.TxBody era ->
  Core.AuxiliaryData era ->
  Tx era
exampleTx mkWitnesses txBody auxData =
  Tx txBody (mkWitnesses txBody keyPairWits) (SJust auxData)
  where
    keyPairWits :: KeyPairWits era
    keyPairWits =
      [ asWitness examplePayKey,
        asWitness exampleStakeKey,
        asWitness $ cold (exampleKeys @(Cardano.Ledger.Era.Crypto era) @'StakePool)
      ]

exampleProposedPParamsUpdates ::
  ( ShelleyBasedEra' era,
    Core.PParamsDelta era ~ PParams' StrictMaybe era
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
  PraosCrypto c =>
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
      sgInitialFunds = Map.empty,
      sgStaking = emptyGenesisStaking
    }

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
  forall era.
  ( ShelleyBasedEra' era,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval
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
      nesPd = examplePoolDistr
    }
  where
    epochState :: EpochState era
    epochState =
      EpochState
        { esAccountState =
            AccountState
              { _treasury = Coin 10000,
                _reserves = Coin 1000
              },
          esSnapshots = emptySnapShots,
          esLState =
            LedgerState
              { lsUTxOState =
                  UTxOState
                    { _utxo =
                        UTxO $
                          SplitMap.fromList
                            [ ( TxIn (TxId (mkDummySafeHash Proxy 1)) minBound,
                                makeTxOut (Proxy @era) addr value
                              )
                            ],
                      _deposited = Coin 1000,
                      _fees = Coin 1,
                      _ppups = def,
                      _stakeDistro = mempty
                    },
                lsDPState = def
              },
          esPrevPp = ppp,
          esPp = pp,
          esNonMyopic = def
        }
      where
        addr :: Addr (Cardano.Ledger.Era.Crypto era)
        addr =
          Addr
            Testnet
            (keyToCredential examplePayKey)
            (StakeRefBase (keyToCredential exampleStakeKey))

    rewardUpdate :: PulsingRewUpdate (Cardano.Ledger.Era.Crypto era)
    (rewardUpdate, _) =
      startStep @era
        (EpochSize 432000)
        (BlocksMade (Map.singleton (mkKeyHash 1) 10))
        epochState
        (Coin 45)
        (activeSlotCoeff testGlobals)
        10

exampleLedgerChainDepState :: forall c. PraosCrypto c => Word64 -> ChainDepState c
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
testEpochInfo = epochInfo testGlobals

mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
mkDummyHash _ = coerce . hashWithSerialiser @h toCBOR

mkDummySafeHash :: forall c a. Cardano.Ledger.Crypto.Crypto c => Proxy c -> Int -> SafeHash c a
mkDummySafeHash _ =
  unsafeMakeSafeHash
    . mkDummyHash (Proxy @(HASH c))

{-------------------------------------------------------------------------------
  Shelley era specific functions
-------------------------------------------------------------------------------}

type StandardShelley = ShelleyEra StandardCrypto

-- | ShelleyLedgerExamples for Shelley era
ledgerExamplesShelley :: ShelleyLedgerExamples StandardShelley
ledgerExamplesShelley =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @StandardShelley))
    id
    exampleCoin
    exampleTxBodyShelley
    exampleAuxiliaryDataShelley
    ()

mkWitnessesPreAlonzo ::
  ShelleyBasedEra' era =>
  Proxy era ->
  Core.TxBody era ->
  KeyPairWits era ->
  WitnessSet era
mkWitnessesPreAlonzo _ txBody keyPairWits =
  mempty
    { addrWits =
        makeWitnessesVKey (coerce (hashAnnotated txBody)) keyPairWits
    }

exampleCoin :: Coin
exampleCoin = Coin 10

exampleTxBodyShelley :: Cardano.Ledger.Shelley.API.TxBody StandardShelley
exampleTxBodyShelley =
  Cardano.Ledger.Shelley.API.TxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ TxOut (mkAddr (examplePayKey, exampleStakeKey)) (Coin 100000)
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

exampleMetadataMap :: Map Word64 Metadatum
exampleMetadataMap =
  Map.fromList
    [ (1, S "string"),
      (2, B "bytes"),
      (3, List [I 1, I 2]),
      (4, Map [(I 3, B "b")])
    ]

exampleAuxiliaryDataShelley :: Core.AuxiliaryData StandardShelley
exampleAuxiliaryDataShelley = Metadata exampleMetadataMap

exampleTxIns :: Cardano.Ledger.Crypto.Crypto c => Set (TxIn c)
exampleTxIns =
  Set.fromList
    [ TxIn (TxId (mkDummySafeHash Proxy 1)) minBound
    ]

exampleCerts :: Cardano.Ledger.Crypto.Crypto c => StrictSeq (DCert c)
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

exampleWithdrawals :: Cardano.Ledger.Crypto.Crypto c => Wdrl c
exampleWithdrawals =
  Wdrl $
    Map.fromList
      [ (_poolRAcnt examplePoolParams, Coin 100)
      ]

exampleProposedPPUpdates ::
  ( Core.PParamsDelta era ~ PParams' StrictMaybe era,
    ShelleyBasedEra' era
  ) =>
  ProposedPPUpdates era
exampleProposedPPUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 1)
      (emptyPParamsUpdate {_maxBHSize = SJust 4000})

examplePayKey :: Cardano.Ledger.Crypto.Crypto c => KeyPair 'Payment c
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: Cardano.Ledger.Crypto.Crypto c => KeyPair 'Staking c
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys :: forall c r. Cardano.Ledger.Crypto.Crypto c => AllIssuerKeys c r
exampleKeys =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @c) 1)
    [(KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3))]
    (hashKey (vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

keyToCredential :: Cardano.Ledger.Crypto.Crypto c => KeyPair r c -> Credential r c
keyToCredential = KeyHashObj . hashKey . vKey

-- | @mkKeyPair'@ from @Test.Cardano.Ledger.Shelley.Utils@ doesn't work for real
-- crypto:
-- <https://github.com/input-output-hk/cardano-ledger-specs/issues/1770>
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

examplePoolParams :: forall c. Cardano.Ledger.Crypto.Crypto c => PoolParams c
examplePoolParams =
  PoolParams
    { _poolId = hashKey $ vKey $ cold poolKeys,
      _poolVrf = hashVerKeyVRF $ snd $ vrf poolKeys,
      _poolPledge = Coin 1,
      _poolCost = Coin 5,
      _poolMargin = unsafeBoundRational 0.1,
      _poolRAcnt = RewardAcnt Testnet (keyToCredential exampleStakeKey),
      _poolOwners = Set.singleton $ hashKey $ vKey exampleStakeKey,
      _poolRelays = StrictSeq.empty,
      _poolMD =
        SJust $
          PoolMetadata
            { _poolMDUrl = fromJust $ textToUrl "consensus.pool",
              _poolMDHash = "{}"
            }
    }
  where
    poolKeys = exampleKeys @c @'StakePool
