{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.OutputValiditySpec (spec) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
import Cardano.Ledger.Allegra (AllegraEra)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (Network (..), ProtVer (..), StrictMaybe (..))
import Cardano.Ledger.Binary (decodeFull, natVersion, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Genesis
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Transition
import Control.Exception (evaluate)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import qualified Data.ListMap as LM
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import System.FS.API.Types (MountPoint (..), mkFsPath)
import System.FS.IO (ioHasFS)
import System.IO.Temp (withSystemTempDirectory)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Examples (testShelleyGenesis)
import qualified Validation

spec :: Spec
spec = describe "CIP-160 output eligibility" $ do
  let payment = KeyHashObj (mkKeyHash 71)
      stake = StakeRefBase (KeyHashObj (mkKeyHash 72))
      protected = AddrProtected Testnet payment StakeRefNull
      pp11 = emptyPParams @DijkstraEra & ppProtocolVersionL .~ ProtVer (natVersion @11) 0
      pp12 = emptyPParams @DijkstraEra & ppProtocolVersionL .~ ProtVer (natVersion @12) 0
  it "rejects internally constructed protection in every historical era, even with future parameters" $ do
    rejects @ShelleyEra protected
    rejects @AllegraEra protected
    rejects @MaryEra protected
    rejects @AlonzoEra protected
    rejects @BabbageEra protected
    rejects @ConwayEra protected
  it "rejects protection before activation and accepts base/enterprise outputs after activation" $ do
    expectRejected pp11 protected
    forM_ [Mainnet, Testnet] $ \network ->
      forM_ [StakeRefNull, stake] $ \stakeRef ->
        Shelley.validateSupportedAddresses
          pp12
          [mkCoinTxOut @DijkstraEra (AddrProtected network payment stakeRef) (Coin 20)]
          `shouldBe` Validation.Success ()
  prop "rejects internally constructed protected pointers after activation" $ \ptr ->
    expectRejected pp12 (AddrProtected Testnet payment (StakeRefPtr ptr))
  prop "keeps ordinary pointer outputs eligible before and after activation" $ \ptr ->
    forM_ [pp11, pp12] $ \pp ->
      Shelley.validateSupportedAddresses
        pp
        [mkCoinTxOut @DijkstraEra (Addr Testnet payment (StakeRefPtr ptr)) (Coin 20)]
        `shouldBe` Validation.Success ()
  it "maps all newly reachable shared failures to top-level and child failures" $ do
    let bad = NES.singleton 0
        shared = Shelley.UnsupportedOutputAddresses bad :: Shelley.ShelleyUtxoPredFailure DijkstraEra
    injectFailure @"UTXO" shared `shouldBe` UnsupportedOutputAddresses bad
    injectFailure @"SUBUTXO" shared `shouldBe` SubUnsupportedOutputAddresses bad
    let allegra = Allegra.UnsupportedOutputAddresses bad :: Allegra.AllegraUtxoPredFailure DijkstraEra
        alonzo = Alonzo.UnsupportedOutputAddresses bad :: Alonzo.AlonzoUtxoPredFailure DijkstraEra
        babbage = Babbage.AlonzoInBabbageUtxoPredFailure alonzo
        conway = Conway.UnsupportedOutputAddresses bad :: Conway.ConwayUtxoPredFailure DijkstraEra
    injectFailure @"UTXO" allegra `shouldBe` UnsupportedOutputAddresses bad
    injectFailure @"UTXO" alonzo `shouldBe` UnsupportedOutputAddresses bad
    injectFailure @"UTXO" babbage `shouldBe` UnsupportedOutputAddresses bad
    injectFailure @"UTXO" conway `shouldBe` UnsupportedOutputAddresses bad
    injectFailure @"SUBUTXO" allegra `shouldBe` SubUnsupportedOutputAddresses bad
    injectFailure @"SUBUTXO" alonzo `shouldBe` SubUnsupportedOutputAddresses bad
    injectFailure @"SUBUTXO" babbage `shouldBe` SubUnsupportedOutputAddresses bad
    injectFailure @"SUBUTXO" conway `shouldBe` SubUnsupportedOutputAddresses bad
    let top = UnsupportedOutputAddresses bad :: DijkstraUtxoPredFailure DijkstraEra
        nested = UtxoFailure top :: DijkstraUtxowPredFailure DijkstraEra
    dijkstraUtxoToDijkstraSubUtxoPredFailure top `shouldBe` Just (SubUnsupportedOutputAddresses bad)
    injectFailure @"SUBUTXOW" nested `shouldBe` SubUtxoFailure (SubUnsupportedOutputAddresses bad)
    dijkstraUtxoToDijkstraSubUtxoPredFailure
      (ProtectedCollateralReturn :: DijkstraUtxoPredFailure DijkstraEra)
      `shouldBe` Nothing
  it "reports each invalid ordinary output at its authored body-local position" $ do
    let ordinary = mkCoinTxOut @DijkstraEra (Addr Testnet payment StakeRefNull) (Coin 20)
        bad = mkCoinTxOut @DijkstraEra protected (Coin 20)
    case Shelley.validateSupportedAddresses pp11 [ordinary, bad, ordinary, bad] of
      Validation.Failure (Shelley.UnsupportedOutputAddresses indexes NE.:| []) ->
        NES.toList indexes `shouldBe` [1, 3]
      _ -> expectationFailure "Expected exactly the two unsupported output positions"
  it "roundtrips the appended top/child failures and a payload-free collateral rejection" $ do
    let version = eraProtVerLow @DijkstraEra
        top = UnsupportedOutputAddresses (NES.singleton 7) :: DijkstraUtxoPredFailure DijkstraEra
        child = SubUnsupportedOutputAddresses (NES.singleton 4) :: DijkstraSubUtxoPredFailure DijkstraEra
        collateral = ProtectedCollateralReturn :: DijkstraUtxoPredFailure DijkstraEra
    decodeFull version (serialize version top) `shouldBe` Right top
    decodeFull version (serialize version child) `shouldBe` Right child
    decodeFull version (serialize version collateral) `shouldBe` Right collateral
    serialize version collateral `shouldBe` BSL.pack [0x81, 0x18, 0x19]
  it "roundtrips preactivation address diagnostics without admitting their transaction bytes" $ do
    let failure =
          Shelley.UnsupportedOutputAddresses (NES.singleton 0) :: Shelley.ShelleyUtxoPredFailure ShelleyEra
        version = eraProtVerLow @ShelleyEra
    decodeFull version (serialize version failure) `shouldBe` Right failure
    decodeFull @Addr version (serialize version protected) `shouldSatisfy` isLeft

  describe "Network initialization" $ do
    let funds = LM.fromList [(protected, Coin 20)]
        genesis = testShelleyGenesis {sgInitialFunds = funds}
        embedded = ShelleyExtraConfig (EmbeddedInjection funds) NoInjection NoInjection
        emptyConfig = mkShelleyTransitionConfig testShelleyGenesis
        initialState = createInitialState emptyConfig
    it "rejects the legacy genesis JSON initial-fund field" $
      (Aeson.eitherDecode (Aeson.encode genesis) :: Either String ShelleyGenesis) `shouldSatisfy` isLeft
    it "rejects embedded extra-config JSON initial funds" $
      (Aeson.eitherDecode (Aeson.encode embedded) :: Either String ShelleyExtraConfig)
        `shouldSatisfy` isLeft
    it "rejects direct genesis UTxO construction from an in-memory config" $
      evaluate (genesisUTxO @DijkstraEra genesis)
        `shouldThrow` (== InjectionProtectedInitialFunds protected)
    it "rejects embedded injection without changing the initial state" $
      registerInitialFunds
        (error "embedded injection does not use HasFS")
        (mkShelleyTransitionConfig genesis)
        initialState
        `shouldThrow` (== InjectionProtectedInitialFunds protected)
    it "rejects streamed injection from a correctly hashed file" $
      withSystemTempDirectory "cip160-initial-funds" $ \dir -> do
        let bytes = Aeson.encode funds
            fileName = "initial-funds.json"
            fileSource = InjectionFromFile (mkFsPath [fileName]) (Hash.hashWith id (BSL.toStrict bytes))
            config =
              mkShelleyTransitionConfig
                testShelleyGenesis {sgExtraConfig = SJust embedded {secInitialFunds = fileSource}}
        BSL.writeFile (dir <> "/" <> fileName) bytes
        registerInitialFunds (ioHasFS (MountPoint dir)) config initialState
          `shouldThrow` (== InjectionProtectedInitialFunds protected)
    it "preserves ordinary genesis funds" $ do
      let ordinary = Addr Testnet payment StakeRefNull
      validateInitialFundAddresses (LM.fromList [(ordinary, Coin 20)]) `shouldBe` Right ()
      validateInitialFundAddresses funds `shouldBe` Left protected

rejects :: forall era. EraTxOut era => Addr -> Expectation
rejects = expectRejected (emptyPParams @era & ppProtocolVersionL .~ ProtVer (natVersion @12) 0)

expectRejected :: forall era. EraTxOut era => PParams era -> Addr -> Expectation
expectRejected pp addr =
  case Shelley.validateSupportedAddresses pp [mkCoinTxOut @era addr (Coin 20)] of
    Validation.Failure (Shelley.UnsupportedOutputAddresses bad NE.:| []) -> bad `shouldBe` NES.singleton 0
    _ -> expectationFailure "Expected exactly the unsupported address failure"
