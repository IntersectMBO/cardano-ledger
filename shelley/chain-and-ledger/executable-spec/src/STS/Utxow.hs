{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Utxow
  ( UTXOW
  , PredicateFailure(..)
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Set as Set
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Functor.Identity (runIdentity)

import           BaseTypes (Globals, ShelleyBase, intervalValue, (==>))
import           Delegation.Certificates (isInstantaneousRewards)
import           Keys
import           Ledger.Core (dom, (∩))
import           LedgerState hiding (genDelegs)
import           PParams (_d)
import           STS.Utxo
import           Tx
import           TxData
import           UTxO
import           Validation (Validity (..))

import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace(..), envGen, sigGen)

import           Hedgehog (Gen)

data UTXOW crypto

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => STS (UTXOW crypto)
 where
  type State (UTXOW crypto) = UTxOState crypto
  type Signal (UTXOW crypto) = Tx crypto
  type Environment (UTXOW crypto) = UtxoEnv crypto
  type BaseM (UTXOW crypto) = ShelleyBase
  data PredicateFailure (UTXOW crypto)
    = InvalidWitnessesUTXOW
    | MissingVKeyWitnessesUTXOW
    | MissingScriptWitnessesUTXOW
    | ScriptWitnessNotValidatingUTXOW
    | UtxoFailure (PredicateFailure (UTXO crypto))
    | MIRInsufficientGenesisSigsUTXOW
    | MIRImpossibleInDecentralizedNetUTXOW
    deriving (Eq, Show)

  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

initialLedgerStateUTXOW
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
   => InitialRule (UTXOW crypto)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakeKeys stakePools genDelegs) <- judgmentContext
  trans @(UTXO crypto) $ IRC (UtxoEnv slots pp stakeKeys stakePools genDelegs)

utxoWitnessed
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
   => TransitionRule (UTXOW crypto)
utxoWitnessed = do
  TRC (UtxoEnv slot pp stakeKeys stakePools _genDelegs, u, tx@(Tx txbody wits _))
    <- judgmentContext
  verifiedWits tx == Valid ?! InvalidWitnessesUTXOW
  let witnessKeys = Set.map witKeyHash wits
  let needed = witsVKeyNeeded (_utxo u) tx _genDelegs
  needed `Set.isSubsetOf` witnessKeys  ?! MissingVKeyWitnessesUTXOW

  -- check multi-signature scripts
  let utxo' = _utxo u

  all (\(hs, validator) -> hashScript validator == hs
      && validateScript validator tx) (Map.toList $ txwitsScript tx)
    ?!ScriptWitnessNotValidatingUTXOW

  scriptsNeeded utxo' tx == Map.keysSet (txwitsScript tx)
    ?! MissingScriptWitnessesUTXOW

  -- check genesis keys signatures for instantaneous rewards certificates
  let mirCerts = Seq.filter isInstantaneousRewards $ _certs txbody
      GenDelegs genMapping = _genDelegs
      genSig = (Set.map undiscriminateKeyHash $ dom genMapping) ∩ Set.map witKeyHash wits
  (    (not $ null mirCerts)
   ==> Set.size genSig >= 5)
      ?! MIRInsufficientGenesisSigsUTXOW
  (    (not $ null mirCerts)
   ==> (0 < intervalValue (_d pp)))
    ?! MIRImpossibleInDecentralizedNetUTXOW

  trans @(UTXO crypto)
    $ TRC (UtxoEnv slot pp stakeKeys stakePools _genDelegs, u, tx)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (UTXO crypto) (UTXOW crypto)
 where
  wrapFailed = UtxoFailure

instance
    ( Crypto crypto
    , Signable (DSIGN crypto) (TxBody crypto)
    )
  => HasTrace (UTXOW crypto) where
  envGen _ = undefined :: Gen (UtxoEnv crypto)
  sigGen _ _ = undefined :: Gen (Tx crypto)

  type BaseEnv (UTXOW crypto) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
