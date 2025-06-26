{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERTS rule
module Test.Cardano.Ledger.Constrained.Conway.Certs where

import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.State
import Constrained.API
import Data.Foldable (toList)
import Data.Sequence (Seq, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Constrained.Conway.Cert
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

-- =======================================================

setMapMaybe :: Ord a => (t -> Maybe a) -> Set t -> Set a
setMapMaybe f set = Set.foldr' (\x s -> maybe s (`Set.insert` s) $ f x) mempty set

txZero :: EraTx era => Tx era
txZero = mkBasicTx mkBasicTxBody

certsEnvSpec ::
  (EraSpecPParams era, HasSpec (Tx era)) =>
  Specification (CertsEnv era)
certsEnvSpec = constrained $ \ce ->
  match ce $ \tx pp _currepoch _currcommittee commproposals ->
    [ satisfies pp pparamsSpec
    , assert $ tx ==. lit txZero
    , genHint 3 commproposals
    ]

-- | Project a CertEnv out of a CertsEnv (i.e drop the Tx)
projectEnv :: CertsEnv era -> CertEnv era
projectEnv x =
  CertEnv
    { cePParams = certsPParams x
    , ceCurrentEpoch = certsCurrentEpoch x
    , ceCurrentCommittee = certsCurrentCommittee x
    , ceCommitteeProposals = certsCommitteeProposals x
    }

txCertsSpec ::
  forall era.
  EraSpecCert era =>
  WitUniv era ->
  CertsEnv era ->
  CertState era ->
  Specification (Seq (TxCert era))
txCertsSpec univ env state =
  constrained $ \seqs ->
    exists
      (\eval -> pure $ toList (eval seqs))
      (\list -> satisfies (pair_ list seqs) (listSeqCertPairSpec @era univ (projectEnv @era env) state))

noSameKeys :: forall era. EraSpecCert era => [TxCert era] -> [TxCert era]
noSameKeys [] = []
noSameKeys (x : xs) = x : noSameKeys @era (filter (\y -> txCertKey @era x /= txCertKey @era y) xs)

-- | Specify a pair of List and Seq, where they have essentially the same elements
--   EXCEPT, the Seq has duplicate keys filtered out.
listSeqCertPairSpec ::
  forall era.
  EraSpecCert era =>
  WitUniv era ->
  CertEnv era ->
  CertState era ->
  Specification ([TxCert era], Seq (TxCert era))
listSeqCertPairSpec univ env state =
  constrained' $ \list seqs ->
    [ assert $ sizeOf_ list <=. 5
    , forAll list $ \x -> satisfies x (txCertSpec @era univ env state)
    , reify list (fromList . noSameKeys @era) (\x -> seqs ==. x)
    ]
