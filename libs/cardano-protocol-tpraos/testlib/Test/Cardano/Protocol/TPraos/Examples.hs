{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.TPraos.Examples (
  tPraosLedgerExamples,
) where
import Cardano.Crypto.Hash as Hash
import Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Keys
import Cardano.Protocol.Crypto (Crypto, StandardCrypto, VRF)
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Cardano.Protocol.TPraos.Rules.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn
import Data.Coerce (Coercible, coerce)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64, Word8)
import Lens.Micro
import Test.Cardano.Ledger.Api.Examples (
  LedgerExamples (..),
  ProtocolLedgerExamples (..),
  protocolLedgerExamples,
 )
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Shelley.Arbitrary (RawSeed (..))
import Test.Cardano.Ledger.Shelley.Examples (
  mkDSIGNKeyPair,
  mkKeyHash,
  seedFromByte,
  seedFromWords,
 )
import Test.Cardano.Protocol.TPraos.Create (
  AllIssuerKeys (..),
  KESKeyPair (..),
  VRFKeyPair (..),
  kesSignKey,
  mkOCert,
 )
tPraosLedgerExamples ::
  forall era.
  EraBlockBody era =>
  LedgerExamples era ->
  ProtocolLedgerExamples (BHeader StandardCrypto) ChainDepState era
tPraosLedgerExamples =
  protocolLedgerExamples
    exampleHashHeader
    exampleBlockHeader
    (exampleChainDepState 1)

exampleHashHeader :: HashHeader
exampleHashHeader = coerce $ mkDummyHash @HASH (0 :: Int)

exampleBlockHeader :: forall era. EraBlockBody era => [Tx era] -> BHeader StandardCrypto
exampleBlockHeader txs = BHeader blockHeaderBody (unsoundPureSignedKES () 0 blockHeaderBody hotKey)
  where
    hotKey = kesSignKey $ snd $ NE.head $ aikHot exampleKeys
    KeyPair vKeyCold _ = aikCold exampleKeys

    blockHeaderBody :: BHBody StandardCrypto
    blockHeaderBody =
      BHBody
        { bheaderBlockNo = BlockNo 3
        , bheaderSlotNo = SlotNo 9
        , bheaderPrev = BlockHash (HashHeader (mkDummyHash (2 :: Int)))
        , bheaderVk = coerceKeyRole vKeyCold
        , bheaderVrfVk = vrfVerKey $ aikVrf exampleKeys
        , bheaderEta = mkCertifiedVRF (mkBytes 0) (vrfSignKey $ aikVrf exampleKeys)
        , bheaderL = mkCertifiedVRF (mkBytes 1) (vrfSignKey $ aikVrf exampleKeys)
        , bsize = 2345
        , bhash = hashBlockBody @era $ blockBody txs
        , bheaderOCert = mkOCert exampleKeys 0 (KESPeriod 0)
        , bprotver = ProtVer (natVersion @2) 0
        }
    mkBytes :: Int -> Cardano.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash @Blake2b_256

blockBody :: EraBlockBody era => [Tx era] -> BlockBody era
blockBody txs = mkBasicBlockBody & txSeqBlockBodyL .~ StrictSeq.fromList txs

exampleChainDepState :: Word64 -> ChainDepState
exampleChainDepState seed =
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

exampleKeys :: AllIssuerKeys StandardCrypto r
exampleKeys =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @StandardCrypto) 1)
    ((KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3)) NE.:| [])
    (hashKey (vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

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

mkKESKeyPair :: Crypto c => RawSeed -> KESKeyPair c
mkKESKeyPair seed =
  let sk = unsoundPureGenKeyKES (seedFromWords seed)
      vk = unsoundPureDeriveVerKeyKES sk
   in KESKeyPair
        { kesSignKey = sk
        , kesVerKey = vk
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
