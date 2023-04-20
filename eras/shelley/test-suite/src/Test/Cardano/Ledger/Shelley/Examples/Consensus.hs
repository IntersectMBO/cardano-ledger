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
import Cardano.Ledger.Crypto
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Keys hiding (KeyPair, vKey)
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API hiding (KeyPair, vKey)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.Translation (emptyFromByronTranslationContext)
import Cardano.Ledger.Shelley.TxWits
import Cardano.Protocol.HeaderCrypto
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Cardano.Protocol.TPraos.Rules.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.EpochInfo
import qualified Data.ByteString as Strict
import Data.Coerce (coerce)
import Data.Default.Class
import qualified Data.List.NonEmpty as NE
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
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.Generator.Core
import Test.Cardano.Ledger.Shelley.Utils hiding (mkVRFKeyPair)

type KeyPairWits era = [KeyPair 'Witness (EraCrypto era)]

{-------------------------------------------------------------------------------
  ShelleyLedgerExamples
-------------------------------------------------------------------------------}

data ShelleyResultExamples era = ShelleyResultExamples
  { srePParams :: PParams era
  , sreProposedPPUpdates :: ProposedPPUpdates era
  , srePoolDistr :: PoolDistr (EraCrypto era)
  , sreNonMyopicRewards ::
      Map
        (Either Coin (Credential 'Staking (EraCrypto era)))
        (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  , sreShelleyGenesis :: ShelleyGenesis (EraCrypto era)
  }

deriving instance
  ( Eq (PParams era)
  , Eq (PParamsUpdate era)
  , Era era
  ) =>
  Eq (ShelleyResultExamples era)

data ShelleyLedgerExamples era hc = ShelleyLedgerExamples
  { sleBlock :: Block (BHeader (EraCrypto era) hc) era
  , sleHashHeader :: HashHeader (EraCrypto era)
  , sleTx :: Tx era
  , sleApplyTxError :: ApplyTxError era
  , sleRewardsCredentials :: Set (Either Coin (Credential 'Staking (EraCrypto era)))
  , sleResultExamples :: ShelleyResultExamples era
  , sleNewEpochState :: NewEpochState era
  , sleChainDepState :: ChainDepState (EraCrypto era)
  , sleTranslationContext :: TranslationContext era
  }

deriving instance
  ( EraTx era
  , HeaderCrypto hc
  , EraGovernance era
  , Eq (TxSeq era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (StashedAVVMAddresses era)
  , Eq (TranslationContext era)
  ) =>
  Eq (ShelleyLedgerExamples era hc)

{-------------------------------------------------------------------------------
  Default constructor
-------------------------------------------------------------------------------}

type ShelleyBasedEra' era =
  ( PraosCrypto (EraCrypto era)
  )

defaultShelleyLedgerExamples ::
  forall era hc.
  ( ShelleyBasedEra' era hc
  , EraSegWits era
  , EraGovernance era
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
  , Default (StashedAVVMAddresses era)
  , ProtVerAtMost era 4
  ) =>
  (TxBody era -> KeyPairWits era -> TxWits era) ->
  (ShelleyTx era -> Tx era) ->
  Value era ->
  TxBody era ->
  TxAuxData era ->
  TranslationContext era ->
  ShelleyLedgerExamples era hc
defaultShelleyLedgerExamples mkWitnesses mkAlonzoTx value txBody auxData translationContext =
  ShelleyLedgerExamples
    { sleBlock = exampleShelleyLedgerBlock (mkAlonzoTx tx)
    , sleHashHeader = exampleHashHeader (Proxy @era) (Proxy @hc)
    , sleTx = mkAlonzoTx tx
    , sleApplyTxError =
        ApplyTxError
          [DelegsFailure $ DelegateeNotRegisteredDELEG @era (mkKeyHash 1)]
    , sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100)
          , Right (ScriptHashObj (mkScriptHash 1))
          , Right (KeyHashObj (mkKeyHash 2))
          ]
    , sleResultExamples = resultExamples
    , sleNewEpochState =
        exampleNewEpochState
          (Proxy @hc)
          value
          emptyPParams
          (emptyPParams & ppMinUTxOValueL .~ Coin 1)
    , sleChainDepState = exampleLedgerChainDepState 1
    , sleTranslationContext = translationContext
    }
  where
    tx = exampleTx (Proxy @hc) mkWitnesses txBody auxData

    resultExamples =
      ShelleyResultExamples
        { srePParams = def
        , sreProposedPPUpdates = exampleProposedPParamsUpdates
        , srePoolDistr = examplePoolDistr (Proxy @(EraCrypto era)) (Proxy @hc)
        , sreNonMyopicRewards = exampleNonMyopicRewards
        , sreShelleyGenesis = testShelleyGenesis
        }

{-------------------------------------------------------------------------------
  Helper constructors
-------------------------------------------------------------------------------}

exampleShelleyLedgerBlock ::
  forall era hc.
  (EraSegWits era, PraosCrypto (EraCrypto era) hc) =>
  Tx era ->
  Block (BHeader (EraCrypto era) hc) era
exampleShelleyLedgerBlock tx = Block blockHeader blockBody
  where
    keys :: AllIssuerKeys (EraCrypto era) hc 'StakePool
    keys = exampleKeys (Proxy @(EraCrypto era)) (Proxy @hc) (Proxy @'StakePool)

    hotKey = kesSignKey $ snd $ NE.head $ aikHot keys
    KeyPair vKeyCold _ = aikCold keys

    blockHeader :: BHeader (EraCrypto era) hc
    blockHeader = BHeader blockHeaderBody (signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: BHBody (EraCrypto era) hc
    blockHeaderBody =
      BHBody
        { bheaderBlockNo = BlockNo 3
        , bheaderSlotNo = SlotNo 9
        , bheaderPrev = BlockHash (HashHeader (mkDummyHash (2 :: Int)))
        , bheaderVk = coerceKeyRole vKeyCold
        , bheaderVrfVk = vrfVerKey $ aikVrf keys
        , bheaderEta = mkCertifiedVRF (mkBytes 0) (vrfSignKey $ aikVrf keys)
        , bheaderL = mkCertifiedVRF (mkBytes 1) (vrfSignKey $ aikVrf keys)
        , bsize = 2345
        , bhash = hashTxSeq @era blockBody
        , bheaderOCert = mkOCert keys 0 (KESPeriod 0)
        , bprotver = ProtVer (natVersion @2) 0
        }

    blockBody = toTxSeq @era (StrictSeq.fromList [tx])

    mkBytes :: Int -> Cardano.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash @Blake2b_256

exampleHashHeader ::
  forall era hc.
  ShelleyBasedEra' era hc =>
  Proxy era ->
  Proxy hc ->
  HashHeader (EraCrypto era)
exampleHashHeader _ _ = coerce $ mkDummyHash @(HASH (EraCrypto era)) (0 :: Int)

mkKeyHash :: forall c discriminator. Crypto c => Int -> KeyHash discriminator c
mkKeyHash = KeyHash . mkDummyHash @(ADDRHASH c)

mkScriptHash :: forall c. Crypto c => Int -> ScriptHash c
mkScriptHash = ScriptHash . mkDummyHash @(ADDRHASH c)

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era hc.
  (EraTx era, HeaderCrypto hc) =>
  Proxy hc ->
  (TxBody era -> KeyPairWits era -> TxWits era) ->
  TxBody era ->
  TxAuxData era ->
  ShelleyTx era
exampleTx _ mkWitnesses txBody auxData =
  ShelleyTx txBody (mkWitnesses txBody keyPairWits) (SJust auxData)
  where
    keyPairWits :: KeyPairWits era
    keyPairWits =
      [ asWitness examplePayKey
      , asWitness exampleStakeKey
      , asWitness $ aikCold (exampleKeys (Proxy @(EraCrypto era)) (Proxy @hc) (Proxy @'StakePool))
      ]

exampleProposedPParamsUpdates ::
  EraPParams era =>
  ProposedPPUpdates era
exampleProposedPParamsUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      (emptyPParamsUpdate & ppuKeyDepositL .~ SJust (Coin 100))

examplePoolDistr ::
  PraosCrypto c hc =>
  Proxy c ->
  Proxy hc ->
  PoolDistr c
examplePoolDistr p q =
  PoolDistr $
    Map.fromList
      [
        ( mkKeyHash 1
        , IndividualPoolStake
            1
            (toPoolStakeVRF $ hashVerKeyVRF $ vrfVerKey (aikVrf (exampleKeys p q (Proxy @'StakePool))))
        )
      ]

exampleNonMyopicRewards ::
  forall c.
  Crypto c =>
  Map
    (Either Coin (Credential 'Staking c))
    (Map (KeyHash 'StakePool c) Coin)
exampleNonMyopicRewards =
  Map.fromList
    [ (Left (Coin 100), Map.singleton (mkKeyHash 2) (Coin 3))
    , (Right (ScriptHashObj (mkScriptHash 1)), Map.empty)
    , (Right (KeyHashObj (mkKeyHash 2)), Map.singleton (mkKeyHash 3) (Coin 9))
    ]

-- | These are dummy values.
testShelleyGenesis :: Crypto c => ShelleyGenesis c
testShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = UTCTime (fromGregorian 2020 5 14) 0
    , sgNetworkMagic = 0
    , sgNetworkId = Testnet
    , -- Chosen to match activeSlotCoeff
      sgActiveSlotsCoeff = unsafeBoundRational 0.9
    , sgSecurityParam = securityParameter testGlobals
    , sgEpochLength = runIdentity $ epochInfoSize testEpochInfo 0
    , sgSlotsPerKESPeriod = slotsPerKESPeriod testGlobals
    , sgMaxKESEvolutions = maxKESEvo testGlobals
    , -- Not important
      sgSlotLength = secondsToNominalDiffTimeMicro 2
    , sgUpdateQuorum = quorum testGlobals
    , sgMaxLovelaceSupply = maxLovelaceSupply testGlobals
    , sgProtocolParams = emptyPParams
    , sgGenDelegs = Map.empty
    , sgInitialFunds = mempty
    , sgStaking = emptyGenesisStaking
    }

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
  forall era hc.
  ( EraTxOut era
  , EraGovernance era
  , ShelleyBasedEra' era hc
  , Default (StashedAVVMAddresses era)
  ) =>
  Proxy hc ->
  Value era ->
  PParams era ->
  PParams era ->
  NewEpochState era
exampleNewEpochState q value ppp pp =
  NewEpochState
    { nesEL = EpochNo 0
    , nesBprev = BlocksMade (Map.singleton (mkKeyHash 1) 10)
    , nesBcur = BlocksMade (Map.singleton (mkKeyHash 2) 3)
    , nesEs = epochState
    , nesRu = SJust rewardUpdate
    , nesPd = examplePoolDistr (Proxy @(EraCrypto era)) q
    , stashedAVVMAddresses = def
    }
  where
    epochState :: EpochState era
    epochState =
      EpochState
        { esAccountState =
            AccountState
              { asTreasury = Coin 10000
              , asReserves = Coin 1000
              }
        , esSnapshots = emptySnapShots
        , esLState =
            LedgerState
              { lsUTxOState =
                  UTxOState
                    { utxosUtxo =
                        UTxO $
                          Map.fromList
                            [
                              ( TxIn (TxId (mkDummySafeHash Proxy 1)) minBound
                              , mkBasicTxOut addr value
                              )
                            ]
                    , utxosDeposited = Coin 1000
                    , utxosFees = Coin 1
                    , utxosGovernance = emptyGovernanceState
                    , utxosStakeDistr = mempty
                    }
              , lsCertState = def
              }
        , esPrevPp = ppp
        , esPp = pp
        , esNonMyopic = def
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

exampleLedgerChainDepState :: forall c. Crypto c => Word64 -> ChainDepState c
exampleLedgerChainDepState seed =
  ChainDepState
    { csProtocol =
        PrtclState
          ( Map.fromList
              [ (mkKeyHash 1, 1)
              , (mkKeyHash 2, 2)
              ]
          )
          (mkNonceFromNumber seed)
          (mkNonceFromNumber seed)
    , csTickn =
        TicknState
          NeutralNonce
          (mkNonceFromNumber seed)
    , csLabNonce =
        mkNonceFromNumber seed
    }

testEpochInfo :: EpochInfo Identity
testEpochInfo = epochInfoPure testGlobals

mkDummySafeHash :: forall c a. Crypto c => Proxy c -> Int -> SafeHash c a
mkDummySafeHash _ =
  unsafeMakeSafeHash
    . mkDummyHash @(HASH c)

{-------------------------------------------------------------------------------
  Shelley era specific functions
-------------------------------------------------------------------------------}

-- | ShelleyLedgerExamples for Shelley era
ledgerExamplesShelley :: ShelleyLedgerExamples Shelley StandardCrypto
ledgerExamplesShelley =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @Shelley))
    id
    exampleCoin
    exampleTxBodyShelley
    exampleAuxiliaryDataShelley
    emptyFromByronTranslationContext

mkWitnessesPreAlonzo ::
  ( EraTx era
  , DSIGN.Signable
      (DSIGN (EraCrypto era))
      (Hash.Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  ) =>
  Proxy era ->
  TxBody era ->
  KeyPairWits era ->
  ShelleyTxWits era
mkWitnessesPreAlonzo _ txBody keyPairWits =
  mempty
    { addrWits =
        mkWitnessesVKey (coerce (hashAnnotated txBody)) keyPairWits
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
    (exampleCerts (Proxy @StandardCrypto))
    (exampleWithdrawals (Proxy @StandardCrypto))
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
    [ (1, S "string")
    , (2, B "bytes")
    , (3, List [I 1, I 2])
    , (4, Map [(I 3, B "b")])
    ]

exampleAuxiliaryDataShelley :: TxAuxData Shelley
exampleAuxiliaryDataShelley = ShelleyTxAuxData exampleAuxDataMap

exampleTxIns :: Crypto c => Set (TxIn c)
exampleTxIns =
  Set.fromList
    [ TxIn (TxId (mkDummySafeHash Proxy 1)) minBound
    ]

exampleCerts :: (Crypto c, HeaderCrypto hc) => Proxy hc -> StrictSeq (DCert c)
exampleCerts p =
  StrictSeq.fromList
    [ DCertDeleg (RegKey (keyToCredential exampleStakeKey))
    , DCertPool (RegPool $ examplePoolParams p)
    , DCertMir $
        MIRCert ReservesMIR $
          StakeAddressesMIR $
            Map.fromList
              [ (keyToCredential (mkDSIGNKeyPair 2), DeltaCoin 110)
              ]
    ]

exampleWithdrawals :: (Crypto c, HeaderCrypto hc) => Proxy hc -> Withdrawals c
exampleWithdrawals p =
  Withdrawals $
    Map.fromList
      [ (ppRewardAcnt (examplePoolParams p), Coin 100)
      ]

exampleProposedPPUpdates ::
  EraPParams era =>
  ProposedPPUpdates era
exampleProposedPPUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 1)
      (emptyPParamsUpdate & ppuMaxBHSizeL .~ SJust 4000)

examplePayKey :: Crypto c => KeyPair 'Payment c
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: Crypto c => KeyPair 'Staking c
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys ::
  forall c hc r.
  (Crypto c, HeaderCrypto hc) =>
  Proxy c ->
  Proxy hc ->
  Proxy r ->
  AllIssuerKeys c hc r
exampleKeys _ _ _ =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @hc) 1)
    ((KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3)) NE.:| [])
    (hashKey (vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

keyToCredential :: Crypto c => KeyPair r c -> Credential r c
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
  HeaderCrypto c =>
  Proxy c ->
  Word8 ->
  VRFKeyPair c
mkVRFKeyPair _ byte = VRFKeyPair sk (VRF.deriveVerKeyVRF sk)
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (VRF.seedSizeVRF (Proxy @(VRF c))))
          byte

    sk = VRF.genKeyVRF seed

examplePoolParams :: forall c hc. (Crypto c, HeaderCrypto hc) => Proxy hc -> PoolParams c
examplePoolParams q =
  PoolParams
    { ppId = hashKey $ vKey $ aikCold poolKeys
    , ppVrf = toPoolStakeVRF $ hashVerKeyVRF $ vrfVerKey $ aikVrf poolKeys
    , ppPledge = Coin 1
    , ppCost = Coin 5
    , ppMargin = unsafeBoundRational 0.1
    , ppRewardAcnt = RewardAcnt Testnet (keyToCredential exampleStakeKey)
    , ppOwners = Set.singleton $ hashKey $ vKey exampleStakeKey
    , ppRelays = StrictSeq.empty
    , ppMetadata =
        SJust $
          PoolMetadata
            { pmUrl = fromJust $ textToUrl "consensus.pool"
            , pmHash = "{}"
            }
    }
  where
    poolKeys = exampleKeys (Proxy @c) q (Proxy @'StakePool)
