{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Cardano.Ledger.Crypto (HASH, ADDRHASH, DSIGN, StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import qualified Cardano.Ledger.Era as Era (Crypto)
import Cardano.Ledger.Keys
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Rules.Delegs
import Cardano.Ledger.Shelley.Rules.Ledger
import Cardano.Ledger.Shelley.Tx
import Cardano.Ledger.Shelley.UTxO
import Cardano.Protocol.HeaderCrypto as CC
import Cardano.Protocol.HeaderKeys
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Cardano.Protocol.TPraos.Rules.Overlay (hashPoolStakeVRF)
import Cardano.Protocol.TPraos.Rules.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.Block
import Cardano.Slotting.EpochInfo
import Cardano.Slotting.Slot
import qualified Data.ByteString as Strict
import Data.Coerce (coerce)
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

data ShelleyLedgerExamples era hcrypto = ShelleyLedgerExamples
  { sleBlock :: Block (BHeader (Era.Crypto era) hcrypto) era,
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

type ShelleyBasedEra' era hcrypto =
  ( Default (State (Core.EraRule "PPUP" era)),
    PraosCrypto (Cardano.Ledger.Era.Crypto era) hcrypto
  )

defaultShelleyLedgerExamples ::
  forall era hcrypto r.
  ( ShelleyBasedEra' era hcrypto,
    EraSegWits era,
    PredicateFailure (Core.EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era,
    PredicateFailure (Core.EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era,
    Core.PParams era ~ ShelleyPParams era,
    Core.PParamsUpdate era ~ ShelleyPParamsUpdate era,
    Default (StashedAVVMAddresses era)
  ) =>
  AllIssuerKeys (Crypto era) hcrypto r ->
  (Core.TxBody era -> KeyPairWits era -> Core.Witnesses era) ->
  (ShelleyTx era -> Core.Tx era) ->
  Core.Value era ->
  Core.TxBody era ->
  Core.AuxiliaryData era ->
  TranslationContext era ->
  ShelleyLedgerExamples era hcrypto
defaultShelleyLedgerExamples keys mkWitnesses mkAlonzoTx value txBody auxData translationContext =
  ShelleyLedgerExamples
    { sleBlock = exampleShelleyLedgerBlock (mkAlonzoTx tx),
      sleHashHeader = exampleHashHeader (Proxy @era) (Proxy @hcrypto),
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
          keys
          value
          emptyPParams
          (emptyPParams {_minUTxOValue = Coin 1}),
      sleChainDepState = exampleLedgerChainDepState 1,
      sleTranslationContext = translationContext
    }
  where
    tx = exampleTx keys mkWitnesses txBody auxData

    resultExamples =
      ShelleyResultExamples
        { srePParams = def,
          sreProposedPPUpdates = exampleProposedPParamsUpdates @era @hcrypto,
          srePoolDistr = examplePoolDistr keys,
          sreNonMyopicRewards = exampleNonMyopicRewards,
          sreShelleyGenesis = testShelleyGenesis
        }

{-------------------------------------------------------------------------------
  Helper constructors
-------------------------------------------------------------------------------}

exampleShelleyLedgerBlock ::
  forall era hcrypto.
  (EraSegWits era, ShelleyBasedEra' era hcrypto) =>
  Core.Tx era ->
  Block (BHeader (Era.Crypto era) hcrypto) era
exampleShelleyLedgerBlock tx = Block blockHeader blockBody
  where
    keys :: AllIssuerKeys (Cardano.Ledger.Era.Crypto era) hcrypto 'StakePool
    keys = exampleKeys

    (_, (hotKey, _)) = head $ hot keys
    KeyPair vKeyCold _ = cold keys

    blockHeader :: BHeader (Cardano.Ledger.Era.Crypto era) hcrypto
    blockHeader = BHeader blockHeaderBody (signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: BHBody (Cardano.Ledger.Era.Crypto era) hcrypto
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
  forall era hcrypto.
  ShelleyBasedEra' era hcrypto =>
  Proxy era ->
  Proxy hcrypto ->
  HashHeader (Cardano.Ledger.Era.Crypto era)
exampleHashHeader _ _ = coerce $ mkDummyHash (Proxy @(HASH (Cardano.Ledger.Era.Crypto era))) 0

mkKeyHash :: forall c discriminator. CC.Crypto c => Int -> KeyHash discriminator c
mkKeyHash = KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

mkScriptHash :: forall c. CC.Crypto c => Int -> ScriptHash c
mkScriptHash = ScriptHash . mkDummyHash (Proxy @(ADDRHASH c))

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era hcrypto r.
  Core.EraTx era =>
  AllIssuerKeys (Crypto era) hcrypto r ->
  (Core.TxBody era -> KeyPairWits era -> Core.Witnesses era) ->
  Core.TxBody era ->
  Core.AuxiliaryData era ->
  ShelleyTx era
exampleTx keys mkWitnesses txBody auxData =
  ShelleyTx txBody (mkWitnesses txBody keyPairWits) (SJust auxData)
  where
    keyPairWits :: KeyPairWits era
    keyPairWits =
      [ asWitness examplePayKey,
        asWitness exampleStakeKey,
        asWitness $ cold keys
      ]

exampleProposedPParamsUpdates ::
  forall era hcrypto.
  ( ShelleyBasedEra' era hcrypto,
    Core.PParamsUpdate era ~ ShelleyPParamsUpdate era
  ) =>
  ProposedPPUpdates era
exampleProposedPParamsUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      (emptyPParamsUpdate {_keyDeposit = SJust (Coin 100)})

examplePoolDistr :: forall c hc r. PraosCrypto c hc => AllIssuerKeys c hc r -> PoolDistr c
examplePoolDistr keys =
  PoolDistr $
    Map.fromList
      [ ( mkKeyHash 1,
          IndividualPoolStake
            1
            (hashPoolStakeVRF (snd (vrf keys)))
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
  forall era hcrypto r.
  ( ShelleyBasedEra' era hcrypto,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval,
    Default (StashedAVVMAddresses era),
    Core.EraTxOut era
  ) =>
  AllIssuerKeys (Crypto era) hcrypto r ->
  Core.Value era ->
  Core.PParams era ->
  Core.PParams era ->
  NewEpochState era
exampleNewEpochState keys value ppp pp =
  NewEpochState
    { nesEL = EpochNo 0,
      nesBprev = BlocksMade (Map.singleton (mkKeyHash 1) 10),
      nesBcur = BlocksMade (Map.singleton (mkKeyHash 2) 3),
      nesEs = epochState,
      nesRu = SJust rewardUpdate,
      nesPd = examplePoolDistr keys,
      stashedAVVMAddresses = def
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
                          Map.fromList
                            [ ( TxIn (TxId (mkDummySafeHash Proxy 1)) minBound,
                                Core.mkBasicTxOut addr value
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

mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
mkDummyHash _ = coerce . hashWithSerialiser @h toCBOR

mkDummySafeHash :: forall c a. CC.Crypto c => Proxy c -> Int -> SafeHash c a
mkDummySafeHash _ =
  unsafeMakeSafeHash
    . mkDummyHash (Proxy @(HASH c))

{-------------------------------------------------------------------------------
  Shelley era specific functions
-------------------------------------------------------------------------------}

type StandardShelley = ShelleyEra StandardCrypto

-- | ShelleyLedgerExamples for Shelley era
ledgerExamplesShelley :: ShelleyLedgerExamples StandardShelley StandardCrypto
ledgerExamplesShelley =
  defaultShelleyLedgerExamples
    exampleKeys
    (mkWitnessesPreAlonzo (Proxy @StandardShelley))
    id
    exampleCoin
    exampleTxBodyShelley
    exampleAuxiliaryDataShelley
    ()

mkWitnessesPreAlonzo ::
  ( Core.EraTx era,
    DSIGN.Signable
      (DSIGN (Era.Crypto era))
      (Hash.Hash (HASH (Era.Crypto era)) Core.EraIndependentTxBody)
  ) =>
  Proxy era ->
  Core.TxBody era ->
  KeyPairWits era ->
  ShelleyWitnesses era
mkWitnessesPreAlonzo _ txBody keyPairWits =
  mempty
    { addrWits =
        makeWitnessesVKey (coerce (hashAnnotated txBody)) keyPairWits
    }

exampleCoin :: Coin
exampleCoin = Coin 10

exampleTxBodyShelley :: ShelleyTxBody StandardShelley
exampleTxBodyShelley =
  ShelleyTxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ ShelleyTxOut (mkAddr (examplePayKey, exampleStakeKey)) (Coin 100000)
        ]
    )
    (exampleCerts (exampleKeys @(Crypto StandardShelley) @StandardCrypto))
    (exampleWithdrawals (exampleKeys @(Crypto StandardShelley) @StandardCrypto))
    (Coin 3)
    (SlotNo 10)
    (SJust (Update (exampleProposedPPUpdates @StandardShelley @StandardCrypto) (EpochNo 0)))
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

exampleTxIns :: CC.Crypto c => Set (TxIn c)
exampleTxIns =
  Set.fromList
    [ TxIn (TxId (mkDummySafeHash Proxy 1)) minBound
    ]

exampleCerts ::
  forall c hc .
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  AllIssuerKeys c hc 'StakePool -> StrictSeq (DCert c)
exampleCerts keys =
  StrictSeq.fromList
    [ DCertDeleg (RegKey (keyToCredential exampleStakeKey)),
      DCertPool (RegPool $ examplePoolParams keys),
      DCertMir $
        MIRCert ReservesMIR $
          StakeAddressesMIR $
            Map.fromList
              [ (keyToCredential (mkDSIGNKeyPair 2), DeltaCoin 110)
              ]
    ]

exampleWithdrawals :: (CC.Crypto c, CC.HeaderCrypto hc) => AllIssuerKeys c hc 'StakePool -> Wdrl c
exampleWithdrawals keys =
  Wdrl $
    Map.fromList
      [ (_poolRAcnt $ examplePoolParams keys, Coin 100)
      ]

exampleProposedPPUpdates ::
  forall era hcrypto.
  ( Core.PParamsUpdate era ~ ShelleyPParamsUpdate era,
    ShelleyBasedEra' era hcrypto
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

exampleKeys :: forall c hc r. (CC.Crypto c, CC.HeaderCrypto hc) => AllIssuerKeys c hc r
exampleKeys =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @hc) 1)
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
  (Cardano.Protocol.HeaderKeys.SignKeyVRF c, Cardano.Protocol.HeaderKeys.VerKeyVRF c)
mkVRFKeyPair _ byte = (sk, VRF.deriveVerKeyVRF sk)
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (VRF.seedSizeVRF (Proxy @(VRF c))))
          byte

    sk = VRF.genKeyVRF seed

examplePoolParams ::
  forall c hc.
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  AllIssuerKeys c hc 'StakePool -> PoolParams c
examplePoolParams poolKeys =
  PoolParams
    { _poolId = hashKey $ vKey $ cold poolKeys,
      _poolVrf = hashPoolStakeVRF $ snd $ vrf poolKeys,
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
  -- where
    -- poolKeys = exampleKeys @c @hc @'StakePool
