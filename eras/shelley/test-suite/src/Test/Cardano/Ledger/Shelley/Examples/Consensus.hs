{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Examples.Consensus (
  ShelleyResultExamples (..),
  ShelleyLedgerExamples (..),
  defaultShelleyLedgerExamples,
  exampleShelleyLedgerBlock,
  exampleHashHeader,
  exampleTx,
  exampleProposedPParamsUpdates,
  examplePoolDistr,
  exampleNonMyopicRewards,
  exampleNewEpochState,
  exampleLedgerChainDepState,
  exampleCoin,
  exampleTxBodyShelley,
  exampleAuxDataMap,
  exampleTxIns,
  exampleCerts,
  exampleWithdrawals,
  exampleProposedPPUpdates,
  examplePayKey,
  exampleStakeKey,
  exampleKeys,
  exampleAuxiliaryDataShelley,
  examplePoolParams,
  ledgerExamplesShelley,
  testShelleyGenesis,

  -- * helper functions used in other examples
  keyToCredential,
  mkKeyHash,
  mkWitnessesPreAlonzo,
  mkScriptHash,

  -- * from Translation
  emptyFromByronTranslationContext,
) where

import Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash as Hash
import Cardano.Crypto.KES as KES
import Cardano.Crypto.Seed as Seed
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (EncCBOR (..), hashWithEncoder)
import Cardano.Ledger.Coin
import Cardano.Ledger.Keys hiding (hashVerKeyVRF)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API hiding (hashVerKeyVRF)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Translation (emptyFromByronTranslationContext)
import Cardano.Ledger.Shelley.TxWits
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Cardano.Protocol.TPraos.Rules.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.EpochInfo
import qualified Data.ByteString as Strict
import Data.Coerce (Coercible, coerce)
import Data.Default
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
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals, unsafeBoundRational)
import Test.Cardano.Ledger.Shelley.Arbitrary (RawSeed (..))
import Test.Cardano.Protocol.TPraos.Create (
  AllIssuerKeys (..),
  KESKeyPair (..),
  VRFKeyPair (..),
  kesSignKey,
  mkOCert,
 )

{-------------------------------------------------------------------------------
  ShelleyLedgerExamples
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  ShelleyLedgerExamples
-------------------------------------------------------------------------------}

data ShelleyResultExamples era = ShelleyResultExamples
  { srePParams :: PParams era
  , sreProposedPPUpdates :: ProposedPPUpdates era
  , srePoolDistr :: PoolDistr
  , sreNonMyopicRewards ::
      Map
        (Either Coin (Credential 'Staking))
        (Map (KeyHash 'StakePool) Coin)
  , sreShelleyGenesis :: ShelleyGenesis
  }

deriving instance
  ( Eq (PParams era)
  , Eq (PParamsUpdate era)
  , Era era
  ) =>
  Eq (ShelleyResultExamples era)

data ShelleyLedgerExamples era = ShelleyLedgerExamples
  { sleBlock :: Block (BHeader StandardCrypto) era
  , sleHashHeader :: HashHeader
  , sleTx :: Tx era
  , sleApplyTxError :: ApplyTxError era
  , sleRewardsCredentials :: Set (Either Coin (Credential 'Staking))
  , sleResultExamples :: ShelleyResultExamples era
  , sleNewEpochState :: NewEpochState era
  , sleChainDepState :: ChainDepState
  , sleTranslationContext :: TranslationContext era
  }

deriving instance
  ( EraTx era
  , EraGov era
  , Eq (TxSeq era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (StashedAVVMAddresses era)
  , Eq (TranslationContext era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (ShelleyLedgerExamples era)

{-------------------------------------------------------------------------------
  Default constructor
-------------------------------------------------------------------------------}

defaultShelleyLedgerExamples ::
  forall era.
  ( EraSegWits era
  , EraGov era
  , EraStake era
  , EraCertState era
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
  , Default (StashedAVVMAddresses era)
  , ProtVerAtMost era 4
  ) =>
  (TxBody era -> [KeyPair 'Witness] -> TxWits era) ->
  (ShelleyTx era -> Tx era) ->
  Value era ->
  TxBody era ->
  TxAuxData era ->
  TranslationContext era ->
  ShelleyLedgerExamples era
defaultShelleyLedgerExamples mkWitnesses mkAlonzoTx value txBody auxData translationContext =
  ShelleyLedgerExamples
    { sleBlock = exampleShelleyLedgerBlock (mkAlonzoTx tx)
    , sleHashHeader = exampleHashHeader
    , sleTx = mkAlonzoTx tx
    , sleApplyTxError =
        ApplyTxError . pure . DelegsFailure $
          DelegateeNotRegisteredDELEG @era (mkKeyHash 1)
    , sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100)
          , Right (ScriptHashObj (mkScriptHash 1))
          , Right (KeyHashObj (mkKeyHash 2))
          ]
    , sleResultExamples = resultExamples
    , sleNewEpochState =
        exampleNewEpochState
          value
          emptyPParams
          (emptyPParams & ppMinUTxOValueL .~ Coin 1)
    , sleChainDepState = exampleLedgerChainDepState 1
    , sleTranslationContext = translationContext
    }
  where
    tx = exampleTx mkWitnesses txBody auxData

    resultExamples =
      ShelleyResultExamples
        { srePParams = def
        , sreProposedPPUpdates = exampleProposedPParamsUpdates
        , srePoolDistr = examplePoolDistr
        , sreNonMyopicRewards = exampleNonMyopicRewards
        , sreShelleyGenesis = testShelleyGenesis
        }

{-------------------------------------------------------------------------------
  Helper constructors
-------------------------------------------------------------------------------}

exampleShelleyLedgerBlock ::
  forall era.
  EraSegWits era =>
  Tx era ->
  Block (BHeader StandardCrypto) era
exampleShelleyLedgerBlock tx = Block blockHeader blockBody
  where
    keys :: AllIssuerKeys StandardCrypto 'StakePool
    keys = exampleKeys

    hotKey = kesSignKey $ snd $ NE.head $ aikHot keys
    KeyPair vKeyCold _ = aikCold keys

    blockHeader :: BHeader StandardCrypto
    blockHeader = BHeader blockHeaderBody (unsoundPureSignedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: BHBody StandardCrypto
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

exampleHashHeader :: HashHeader
exampleHashHeader = coerce $ mkDummyHash @HASH (0 :: Int)

mkKeyHash :: forall discriminator. Int -> KeyHash discriminator
mkKeyHash = KeyHash . mkDummyHash @ADDRHASH

mkScriptHash :: Int -> ScriptHash
mkScriptHash = ScriptHash . mkDummyHash @ADDRHASH

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era.
  (TxBody era -> [KeyPair 'Witness] -> TxWits era) ->
  TxBody era ->
  TxAuxData era ->
  ShelleyTx era
exampleTx mkWitnesses txBody auxData =
  ShelleyTx txBody (mkWitnesses txBody keyPairWits) (SJust auxData)
  where
    keyPairWits =
      [ asWitness examplePayKey
      , asWitness exampleStakeKey
      , asWitness $ aikCold exampleKeys
      ]

exampleProposedPParamsUpdates ::
  EraPParams era =>
  ProposedPPUpdates era
exampleProposedPParamsUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      (emptyPParamsUpdate & ppuKeyDepositL .~ SJust (Coin 100))

examplePoolDistr :: PoolDistr
examplePoolDistr =
  PoolDistr
    ( Map.fromList
        [
          ( mkKeyHash 1
          , IndividualPoolStake
              1
              (CompactCoin 1)
              (hashVerKeyVRF @StandardCrypto (vrfVerKey (aikVrf exampleKeys)))
          )
        ]
    )
    (CompactCoin 1)

exampleNonMyopicRewards ::
  Map
    (Either Coin (Credential 'Staking))
    (Map (KeyHash 'StakePool) Coin)
exampleNonMyopicRewards =
  Map.fromList
    [ (Left (Coin 100), Map.singleton (mkKeyHash 2) (Coin 3))
    , (Right (ScriptHashObj (mkScriptHash 1)), Map.empty)
    , (Right (KeyHashObj (mkKeyHash 2)), Map.singleton (mkKeyHash 3) (Coin 9))
    ]

-- | These are dummy values.
testShelleyGenesis :: ShelleyGenesis
testShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = UTCTime (fromGregorian 2020 5 14) 0
    , sgNetworkMagic = 0
    , sgNetworkId = Testnet
    , -- Chosen to match activeSlotCoeff
      sgActiveSlotsCoeff = unsafeBoundRational 0.9
    , sgSecurityParam = securityParameter testGlobals
    , sgEpochLength = runIdentity $ epochInfoSize testEpochInfo (EpochNo 0)
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
  forall era.
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  , Default (StashedAVVMAddresses era)
  ) =>
  Value era ->
  PParams era ->
  PParams era ->
  NewEpochState era
exampleNewEpochState value ppp pp =
  NewEpochState
    { nesEL = EpochNo 0
    , nesBprev = BlocksMade (Map.singleton (mkKeyHash 1) 10)
    , nesBcur = BlocksMade (Map.singleton (mkKeyHash 2) 3)
    , nesEs = epochState
    , nesRu = SJust rewardUpdate
    , nesPd = examplePoolDistr
    , stashedAVVMAddresses = def
    }
  where
    epochState :: EpochState era
    epochState =
      EpochState
        { esChainAccountState =
            ChainAccountState
              { casTreasury = Coin 10000
              , casReserves = Coin 1000
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
                              ( TxIn (TxId (mkDummySafeHash @EraIndependentTxBody 1)) minBound
                              , mkBasicTxOut addr value
                              )
                            ]
                    , utxosDeposited = Coin 1000
                    , utxosFees = Coin 1
                    , utxosGovState = emptyGovState
                    , utxosInstantStake = mempty
                    , utxosDonation = mempty
                    }
              , lsCertState = def
              }
        , esNonMyopic = def
        }
        & prevPParamsEpochStateL .~ ppp
        & curPParamsEpochStateL .~ pp
      where
        addr :: Addr
        addr =
          Addr
            Testnet
            (keyToCredential examplePayKey)
            (StakeRefBase (keyToCredential exampleStakeKey))

    rewardUpdate :: PulsingRewUpdate
    rewardUpdate =
      startStep @era
        (EpochSize 432000)
        (BlocksMade (Map.singleton (mkKeyHash 1) 10))
        epochState
        (Coin 1000)
        (activeSlotCoeff testGlobals)
        (knownNonZeroBounded @10)

exampleLedgerChainDepState :: Word64 -> ChainDepState
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

{-------------------------------------------------------------------------------
  Shelley era specific functions
-------------------------------------------------------------------------------}

-- | ShelleyLedgerExamples for Shelley era
ledgerExamplesShelley :: ShelleyLedgerExamples ShelleyEra
ledgerExamplesShelley =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @ShelleyEra))
    id
    exampleCoin
    exampleTxBodyShelley
    exampleAuxiliaryDataShelley
    emptyFromByronTranslationContext

mkWitnessesPreAlonzo ::
  EraTx era =>
  Proxy era ->
  TxBody era ->
  [KeyPair 'Witness] ->
  ShelleyTxWits era
mkWitnessesPreAlonzo _ txBody keyPairWits =
  mempty
    { addrWits =
        mkWitnessesVKey (coerce (hashAnnotated txBody)) keyPairWits
    }

exampleCoin :: Coin
exampleCoin = Coin 10

exampleTxBodyShelley :: TxBody ShelleyEra
exampleTxBodyShelley =
  ShelleyTxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ ShelleyTxOut (mkAddr examplePayKey exampleStakeKey) (Coin 100000)
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
    auxiliaryDataHash :: TxAuxDataHash
    auxiliaryDataHash =
      TxAuxDataHash $ mkDummySafeHash @EraIndependentTxAuxData 30

exampleAuxDataMap :: Map Word64 Metadatum
exampleAuxDataMap =
  Map.fromList
    [ (1, S "string")
    , (2, B "bytes")
    , (3, List [I 1, I 2])
    , (4, Map [(I 3, B "b")])
    ]

exampleAuxiliaryDataShelley :: TxAuxData ShelleyEra
exampleAuxiliaryDataShelley = ShelleyTxAuxData exampleAuxDataMap

exampleTxIns :: Set TxIn
exampleTxIns =
  Set.fromList
    [ TxIn (TxId (mkDummySafeHash @EraIndependentTxBody 1)) minBound
    ]

exampleCerts :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => StrictSeq (TxCert era)
exampleCerts =
  StrictSeq.fromList
    [ RegTxCert (keyToCredential exampleStakeKey)
    , RegPoolTxCert examplePoolParams
    , MirTxCert $
        MIRCert ReservesMIR $
          StakeAddressesMIR $
            Map.fromList
              [ (keyToCredential (mkDSIGNKeyPair 2), DeltaCoin 110)
              ]
    ]

exampleWithdrawals :: Withdrawals
exampleWithdrawals =
  Withdrawals $
    Map.fromList
      [ (ppRewardAccount examplePoolParams, Coin 100)
      ]

exampleProposedPPUpdates ::
  EraPParams era =>
  ProposedPPUpdates era
exampleProposedPPUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 1)
      (emptyPParamsUpdate & ppuMaxBHSizeL .~ SJust 4000)

examplePayKey :: KeyPair 'Payment
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: KeyPair 'Staking
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys :: AllIssuerKeys StandardCrypto r
exampleKeys =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @StandardCrypto) 1)
    ((KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3)) NE.:| [])
    (hashKey (vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

keyToCredential :: KeyPair r -> Credential r
keyToCredential = KeyHashObj . hashKey . vKey

-- | @mkKeyPair'@ from @Test.Cardano.Ledger.Shelley.Utils@ doesn't work for real
-- crypto:
-- <https://github.com/intersectmbo/cardano-ledger/issues/1770>
mkDSIGNKeyPair :: forall kd. Word8 -> KeyPair kd
mkDSIGNKeyPair byte = KeyPair (VKey $ DSIGN.deriveVerKeyDSIGN sk) sk
  where
    sk = DSIGN.genKeyDSIGN $ seedFromByte byte size
    size = fromIntegral $ DSIGN.seedSizeDSIGN (Proxy @DSIGN)

mkKESKeyPair :: Crypto c => RawSeed -> KESKeyPair c
mkKESKeyPair seed =
  let sk = unsoundPureGenKeyKES (seedFromWords seed)
      vk = unsoundPureDeriveVerKeyKES sk
   in KESKeyPair
        { kesSignKey = sk
        , kesVerKey = vk
        }

mkVRFKeyPair ::
  forall c.
  Crypto c =>
  Proxy c ->
  Word8 ->
  VRFKeyPair c
mkVRFKeyPair _ byte = VRFKeyPair sk (VRF.deriveVerKeyVRF sk)
  where
    sk = VRF.genKeyVRF $ seedFromByte byte size
    size = fromIntegral $ VRF.seedSizeVRF (Proxy @(VRF c))

examplePoolParams :: PoolParams
examplePoolParams =
  PoolParams
    { ppId = hashKey $ vKey $ aikCold exampleKeys
    , ppVrf = hashVerKeyVRF @StandardCrypto $ vrfVerKey $ aikVrf exampleKeys
    , ppPledge = Coin 1
    , ppCost = Coin 5
    , ppMargin = unsafeBoundRational 0.1
    , ppRewardAccount = RewardAccount Testnet (keyToCredential exampleStakeKey)
    , ppOwners = Set.singleton $ hashKey $ vKey exampleStakeKey
    , ppRelays = StrictSeq.empty
    , ppMetadata =
        SJust $
          PoolMetadata
            { pmUrl = fromJust $ textToUrl 64 "consensus.pool"
            , pmHash = "{}"
            }
    }

mkCertifiedVRF ::
  ( VRF.Signable v a
  , VRF.VRFAlgorithm v
  , VRF.ContextVRF v ~ ()
  , Coercible b (VRF.CertifiedVRF v a)
  ) =>
  a ->
  VRF.SignKeyVRF v ->
  b
mkCertifiedVRF a sk =
  coerce $ VRF.evalCertified () a sk

seedFromByte :: Word8 -> Int -> Seed.Seed
seedFromByte byte size =
  Seed.mkSeedFromBytes $
    Strict.replicate
      size
      byte

seedFromWords :: RawSeed -> Seed.Seed
seedFromWords ws =
  Seed.mkSeedFromBytes . hashToBytes $ hashWithEncoder @HASH shelleyProtVer encCBOR ws
