{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  ledgerExamples,
  -- functions used in building examples for other eras
  mkShelleyBasedLedgerExamples,
  mkShelleyBasedExampleTx,
  exampleShelleyBasedShelleyTxBody,
  exampleShelleyBasedTxBody,
  exampleCerts,
  exampleWithdrawals,
  exampleAuxDataMap,
  exampleNonMyopicRewards,
  exampleCoin,
  examplePayKey,
  exampleStakeKey,
  exampleNewEpochState,
  examplePoolDistr,
  exampleStakePoolParams,
  exampleTxIns,
  exampleProposedPPUpdates,
  exampleByronAddress,
  testShelleyGenesis,
  -- utility functions
  keyToCredential,
  mkDSIGNKeyPair,
  mkKeyHash,
  mkScriptHash,
  mkShelleyBasedWitnesses,
  seedFromByte,
  seedFromWords,
) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed as Seed
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as Byron
import Cardano.Ledger.Address (BootstrapAddress (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (EncCBOR (..), hashWithEncoder)
import Cardano.Ledger.Coin
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Translation (emptyFromByronTranslationContext)
import Cardano.Slotting.EpochInfo
import qualified Data.ByteString as Strict
import Data.Coerce (coerce)
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.MemPack.Buffer (byteArrayFromShortByteString)
import Data.Proxy (Proxy (Proxy))
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

data LedgerExamples era = LedgerExamples
  { -- tx
    leTx :: Tx TopTx era
  , leApplyTxError :: ApplyTxError era
  , -- protocol parameters
    lePParams :: PParams era
  , leProposedPPUpdates :: ProposedPPUpdates era
  , -- Ledger state
    leNewEpochState :: NewEpochState era
  , lePoolDistr :: PoolDistr
  , -- rewards and delegation
    leRewardsCredentials :: Set (Either Coin (Credential Staking))
  , leNonMyopicRewards ::
      Map
        (Either Coin (Credential Staking))
        (Map (KeyHash StakePool) Coin)
  , leTranslationContext :: TranslationContext era
  , leShelleyGenesis :: ShelleyGenesis
  }

deriving instance
  ( EraTx era
  , Eq (PParams era)
  , Eq (PParamsUpdate era)
  , EraGov era
  , Eq (Tx TopTx era)
  , Eq (ApplyTxError era)
  , Eq (StashedAVVMAddresses era)
  , Eq (TranslationContext era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (LedgerExamples era)

ledgerExamples :: LedgerExamples ShelleyEra
ledgerExamples =
  mkShelleyBasedLedgerExamples
    ( ShelleyApplyTxError . pure . DelegsFailure . DelplFailure . DelegFailure $
        DelegateeNotRegisteredDELEG @ShelleyEra (mkKeyHash 1)
    )
    exampleCoin
    (mkShelleyBasedExampleTx $ exampleTxBodyShelley $ Coin 100000)
    emptyFromByronTranslationContext

mkShelleyBasedLedgerExamples ::
  forall era.
  ( EraTx era
  , EraGov era
  , EraStake era
  , EraCertState era
  , Default (StashedAVVMAddresses era)
  , AtMostEra "Mary" era
  ) =>
  ApplyTxError era ->
  Value era ->
  Tx TopTx era ->
  TranslationContext era ->
  LedgerExamples era
mkShelleyBasedLedgerExamples
  applyTxError
  value
  tx
  translationContext =
    LedgerExamples
      { leTx = tx
      , leApplyTxError = applyTxError
      , lePParams = def
      , leProposedPPUpdates =
          ProposedPPUpdates $
            Map.singleton
              (mkKeyHash 0)
              (emptyPParamsUpdate & ppuKeyDepositL .~ SJust (Coin 100))
      , leNewEpochState =
          exampleNewEpochState
            value
            emptyPParams
            (emptyPParams & ppMinUTxOValueL .~ Coin 1)
      , lePoolDistr = examplePoolDistr
      , leRewardsCredentials =
          Set.fromList
            [ Left (Coin 100)
            , Right (ScriptHashObj (mkScriptHash 1))
            , Right (KeyHashObj (mkKeyHash 2))
            ]
      , leNonMyopicRewards = exampleNonMyopicRewards
      , leTranslationContext = translationContext
      , leShelleyGenesis = testShelleyGenesis
      }

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
mkShelleyBasedExampleTx ::
  forall era.
  EraTx era =>
  TxBody TopTx era ->
  Tx TopTx era
mkShelleyBasedExampleTx txBody =
  mkBasicTx @era txBody
    & witsTxL
      .~ mkShelleyBasedWitnesses txBody keyPairWits
    & auxDataTxL .~ SJust exampleAuxiliaryDataShelley
  where
    keyPairWits =
      [ asWitness examplePayKey
      , asWitness exampleStakeKey
      , asWitness $ mkDSIGNKeyPair 1
      ]

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

examplePoolDistr :: PoolDistr
examplePoolDistr =
  PoolDistr
    ( Map.fromList
        [
          ( mkKeyHash 1
          , IndividualPoolStake
              1
              (CompactCoin 1)
              exampleVrfVerKeyHash
          )
        ]
    )
    (knownNonZeroCoin @1)

exampleNonMyopicRewards :: Map (Either Coin (Credential Staking)) (Map (KeyHash StakePool) Coin)
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
    , sgEpochLength = runIdentity $ epochInfoSize (epochInfoPure testGlobals) (EpochNo 0)
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
    , sgExtraConfig = SNothing
    }

exampleCoin :: Coin
exampleCoin = Coin 10

exampleTxBodyShelley :: Value ShelleyEra -> TxBody TopTx ShelleyEra
exampleTxBodyShelley value =
  exampleShelleyBasedShelleyTxBody value
    & ttlTxBodyL .~ SlotNo 10

-- | Shelley era TxBody which is compatible with TxBody of future eras. Doesn't have
-- the ttl field, since that field only exists in Shelley era. For an example
-- which includes the ttl field, use 'exampleTxBodyShelley'.
exampleShelleyBasedShelleyTxBody ::
  forall era.
  ShelleyEraTxBody era =>
  Value era ->
  TxBody TopTx era
exampleShelleyBasedShelleyTxBody value =
  exampleShelleyBasedTxBody value
    & certsTxBodyL .~ exampleCerts
    & updateTxBodyL .~ SJust (Update exampleProposedPPUpdates (EpochNo 0))

exampleShelleyBasedTxBody ::
  forall era.
  EraTxBody era =>
  Value era ->
  TxBody TopTx era
exampleShelleyBasedTxBody value =
  mkBasicTxBody
    & inputsTxBodyL .~ exampleTxIns
    & outputsTxBodyL
      .~ StrictSeq.fromList
        [ mkBasicTxOut (mkAddr examplePayKey exampleStakeKey) value
        ]
    & withdrawalsTxBodyL .~ exampleWithdrawals
    & feeTxBodyL .~ Coin 3
    & auxDataHashTxBodyL .~ SJust exampleAuxDataHash
  where
    exampleAuxDataHash = TxAuxDataHash $ mkDummySafeHash @EraIndependentTxAuxData 30

exampleAuxiliaryDataShelley :: EraTxAuxData era => TxAuxData era
exampleAuxiliaryDataShelley =
  mkBasicTxAuxData & metadataTxAuxDataL .~ exampleAuxDataMap

exampleAuxDataMap :: Map Word64 Metadatum
exampleAuxDataMap =
  Map.fromList
    [ (1, S "string")
    , (2, B $ byteArrayFromShortByteString "bytes")
    , (3, List [I 1, I 2])
    , (4, Map [(I 3, B $ byteArrayFromShortByteString "b")])
    ]

exampleTxIns :: Set TxIn
exampleTxIns =
  Set.fromList
    [ TxIn (TxId (mkDummySafeHash @EraIndependentTxBody 1)) minBound
    ]

exampleCerts :: (ShelleyEraTxCert era, AtMostEra "Babbage" era) => StrictSeq (TxCert era)
exampleCerts =
  StrictSeq.fromList
    [ RegTxCert (keyToCredential exampleStakeKey)
    , RegPoolTxCert exampleStakePoolParams
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
      [ (exampleAccountAddress, Coin 100)
      ]

exampleProposedPPUpdates ::
  EraPParams era =>
  ProposedPPUpdates era
exampleProposedPPUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 1)
      (emptyPParamsUpdate & ppuMaxBHSizeL .~ SJust 4000)

exampleStakePoolParams :: StakePoolParams
exampleStakePoolParams =
  StakePoolParams
    { sppId = hashKey $ vKey $ mkDSIGNKeyPair 1
    , sppVrf = exampleVrfVerKeyHash
    , sppPledge = Coin 1
    , sppCost = Coin 5
    , sppMargin = unsafeBoundRational 0.1
    , sppAccountAddress = exampleAccountAddress
    , sppOwners = Set.singleton $ hashKey $ vKey exampleStakeKey
    , sppRelays = StrictSeq.empty
    , sppMetadata =
        SJust $
          PoolMetadata
            { pmUrl = fromJust $ textToUrl 64 "consensus.pool"
            , pmHash = byteArrayFromShortByteString "{}"
            }
    }

examplePayKey :: KeyPair Payment
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: KeyPair Staking
exampleStakeKey = mkDSIGNKeyPair 1

exampleVrfVerKeyHash :: VRFVerKeyHash StakePoolVRF
exampleVrfVerKeyHash = VRFVerKeyHash "c5e21ab1c9f6022d81c3b25e3436cb7f1df77f9652ae3e1310c28e621dd87b4c"

exampleAccountAddress :: AccountAddress
exampleAccountAddress = AccountAddress Testnet (AccountId (keyToCredential exampleStakeKey))

exampleByronAddress :: Addr
exampleByronAddress = AddrBootstrap (BootstrapAddress byronAddr)
  where
    byronAddr = Byron.makeAddress asd attrs
    asd = Byron.VerKeyASD byronVerificationKey
    attrs =
      Byron.AddrAttributes
        (Just (Byron.HDAddressPayload "a compressed lenna.png"))
        (Byron.NetworkTestnet 0)
    byronVerificationKey = Byron.toVerification signingKey
    signingKey = Byron.SigningKey $ Byron.generate seed (mempty :: ByteString)
    seed = "12345678901234567890123456789012" :: ByteString

mkShelleyBasedWitnesses ::
  EraTx era =>
  TxBody TopTx era ->
  [KeyPair Witness] ->
  TxWits era
mkShelleyBasedWitnesses txBody keyPairWits =
  mkBasicTxWits
    & addrTxWitsL .~ mkWitnessesVKey (coerce (txIdTxBody txBody)) keyPairWits
    & bootAddrTxWitsL .~ mempty

-- | @mkKeyPair'@ from @Test.Cardano.Ledger.Shelley.Utils@ doesn't work for real
-- crypto:
-- <https://github.com/intersectmbo/cardano-ledger/issues/1770>
mkDSIGNKeyPair :: forall kd. Word8 -> KeyPair kd
mkDSIGNKeyPair byte = KeyPair (VKey $ DSIGN.deriveVerKeyDSIGN sk) sk
  where
    sk = DSIGN.genKeyDSIGN $ seedFromByte byte size
    size = fromIntegral $ DSIGN.seedSizeDSIGN (Proxy @DSIGN)

mkKeyHash :: forall discriminator. Int -> KeyHash discriminator
mkKeyHash = KeyHash . mkDummyHash @ADDRHASH

mkScriptHash :: Int -> ScriptHash
mkScriptHash = ScriptHash . mkDummyHash @ADDRHASH

seedFromByte :: Word8 -> Int -> Seed.Seed
seedFromByte byte size =
  Seed.mkSeedFromBytes $
    Strict.replicate
      size
      byte

seedFromWords :: RawSeed -> Seed.Seed
seedFromWords ws =
  Seed.mkSeedFromBytes . hashToBytes $ hashWithEncoder @HASH shelleyProtVer encCBOR ws

keyToCredential :: KeyPair r -> Credential r
keyToCredential = KeyHashObj . hashKey . vKey
