{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Ssc.Example
       ( exampleCommitment
       , exampleCommitmentsMap
       , exampleCommitmentSignature
       , exampleInnerSharesMap
       , exampleOpening
       , exampleOpeningsMap
       , exampleSignedCommitment
       , exampleVssCertificate
       , exampleVssCertificatesMap
       , randCommitmentAndOpening
       ) where

import           Cardano.Prelude

import qualified Crypto.Random as Rand
import qualified Crypto.SCRAPE as Scrape
import           Data.Coerce (coerce)
import           Data.List ((!!))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Formatting (build, sformat, (%))

import           Cardano.Binary.Class (asBinary)
import           Cardano.Chain.Common (mkStakeholderId)
import           Cardano.Chain.Slotting (EpochIndex (..))
import           Cardano.Chain.Ssc (Commitment (..), CommitmentSignature,
                     CommitmentsMap, InnerSharesMap, Opening (..), OpeningsMap,
                     SignedCommitment, VssCertificate (..),
                     VssCertificatesMap (..), mkCommitmentsMap,
                     mkVssCertificate, mkVssCertificatesMap)
import           Cardano.Crypto (EncShare (..), ProtocolMagic (..), Secret (..),
                     SecretProof (..), SignTag (..), Threshold, VssKeyPair,
                     VssPublicKey (..), decryptShare, deterministic,
                     deterministicVssKeyGen, genSharedSecret, sign,
                     toVssPublicKey)

import           Test.Cardano.Chain.Common.Example (exampleStakeholderId)
import           Test.Cardano.Chain.Slotting.Example (exampleEpochIndex)
import           Test.Cardano.Crypto.Bi (getBytes)
import           Test.Cardano.Crypto.Example (examplePublicKey,
                     examplePublicKeys, exampleSecretKey, exampleSecretKeys)


exampleCommitment :: Commitment
exampleCommitment = fst exampleCommitmentOpening

exampleCommitmentOpening :: (Commitment, Opening)
exampleCommitmentOpening =
    let
        numKeys   = 128 :: Int
        threshold = 15 :: Integer
        vssKeys   = replicate numKeys exampleVssPublicKey
    in deterministic "commitmentOpening"
        $ randCommitmentAndOpening threshold (NE.fromList vssKeys)

exampleCommitmentSignature :: CommitmentSignature
exampleCommitmentSignature = sign
    (ProtocolMagic 0)
    SignForTestingOnly
    exampleSecretKey
    (exampleEpochIndex, exampleCommitment)

exampleCommitmentsMap :: CommitmentsMap
exampleCommitmentsMap =
    let
        numCommitments    = 1
        signedCommitments = replicate numCommitments exampleSignedCommitment
    in mkCommitmentsMap signedCommitments

exampleOpening :: Opening
exampleOpening = snd exampleCommitmentOpening

exampleOpeningsMap :: OpeningsMap
exampleOpeningsMap =
    let
        mapSize        = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        openings       = replicate mapSize exampleOpening
    in Map.fromList $ zip stakeholderIds openings

exampleSignedCommitment :: SignedCommitment
exampleSignedCommitment =
    (examplePublicKey, exampleCommitment, exampleCommitmentSignature)

-- Lifted from genSharedSecret in `Cardano.Crypto.SecretSharing`.
-- added `deterministic` in third guard.

exampleSharedSecret
    :: Scrape.Threshold
    -> NonEmpty VssPublicKey
    -> (Secret, SecretProof, [(VssPublicKey, EncShare)])
exampleSharedSecret t ps
    | t <= 1
    = error "genSharedSecret: threshold must be > 1"
    | t >= n - 1
    = error "genSharedSecret: threshold must be > n-1"
    | otherwise
    = convertRes . deterministic "ss" $ Scrape.escrow t (coerce sorted)
  where
    n      = fromIntegral (length ps)
    sorted = sort (toList ps)
    convertRes (gen, secret, shares, comms, proof, pproofs) =
        ( coerce secret
        , SecretProof gen proof pproofs comms
        , zip sorted (coerce shares)
        )

-- Not sure why you don't use `VssPublicKey` for the `InnerSharesMap`
-- as you use `VssPublicKey`s to generate `DecShare`s.

exampleInnerSharesMap :: Scrape.Threshold -> Int -> InnerSharesMap
exampleInnerSharesMap count offset = Map.fromList $ zipWith
    (\x y -> (mkStakeholderId x, NE.fromList [asBinary y]))
    pubKeys
    decShares
  where
          -- generate VssPublicKey and VssSecretKey pairs.
    vssKeyPairs = exampleVssKeyPairs offset $ fromIntegral (count + 1)
    -- generate SharedSecrets from the VssPublicKeys generated above.
    ss          = exampleSharedSecret
        count
        (NE.fromList $ map toVssPublicKey vssKeyPairs)
    -- filter `VssPublicKeys`s and their corresponding `EncShare`s.
    encShares (_, _, pKeSlist) = map snd pKeSlist
    -- generate `PublicKey`s
    pubKeys = examplePublicKeys 1 $ fromIntegral (count + 1)
    -- generate `DecShares`
    decShares =
        [ deterministic "ss" $ decryptShare pr es
        | pr <- vssKeyPairs
        , es <- encShares ss
        ]

exampleVssKeyPairs :: Int -> Int -> [VssKeyPair]
exampleVssKeyPairs offset count = map (toPair . (* offset)) [0 .. count]
    where toPair start = deterministicVssKeyGen (getBytes start 32)

exampleVssPublicKey :: VssPublicKey
exampleVssPublicKey = toVssPublicKey mkVssKeyPair
    where mkVssKeyPair = deterministicVssKeyGen $ (getBytes 0 32)

exampleVssCertificate :: VssCertificate
exampleVssCertificate = mkVssCertificate
    (ProtocolMagic 0)
    exampleSecretKey
    (asBinary (toVssPublicKey $ deterministicVssKeyGen ("golden" :: ByteString))
    )
    (EpochIndex 11)

exampleVssCertificates :: Int -> Int -> [VssCertificate]
exampleVssCertificates offset num = map vssCert [0 .. num - 1]
  where
    secretKeyList = (exampleSecretKeys offset num)
    vssCert index = mkVssCertificate
        (ProtocolMagic 0)
        (secretKeyList !! index)
        (asBinary
            (toVssPublicKey $ deterministicVssKeyGen (getBytes index 128))
        )
        (EpochIndex 122)

exampleVssCertificatesMap :: Int -> Int -> VssCertificatesMap
exampleVssCertificatesMap offset num =
    mkVssCertificatesMap $ exampleVssCertificates offset num

-- | Generate random SharedSeed
randCommitmentAndOpening
    :: Rand.MonadRandom m
    => Threshold
    -> NonEmpty VssPublicKey
    -> m (Commitment, Opening)
randCommitmentAndOpening t pks
    | t <= 1 = error $ sformat
        ("randCommitmentAndOpening: threshold (" % build % ") must be > 1")
        t
    | t >= n - 1 = error $ sformat
        ( "randCommitmentAndOpening: threshold ("
        % build
        % ") must be < n-1"
        % " (n = "
        % build
        % ")"
        )
        t
        n
    | otherwise = convertRes <$> genSharedSecret t pks
  where
    n = fromIntegral (length pks)
    convertRes (secret, proof, shares) =
        ( Commitment
            { commProof  = proof
            , commShares = Map.fromList $ map toPair $ NE.groupWith
                fst
                shares
            }
        , Opening $ asBinary secret
        )
    toPair ne@(x :| _) = (asBinary (fst x), NE.map (asBinary . snd) ne)
