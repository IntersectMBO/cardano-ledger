{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.EraGen (genesisId) where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import Data.List (foldl')
import qualified Data.List as List ((\\))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API
  ( Addr (Addr),
    Coin (..),
    Credential (ScriptHashObj),
    DCert,
    StakeReference (StakeRefBase),
    Update,
  )
import Shelley.Spec.Ledger.Address (toAddr)
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys (KeyPair (..), KeyRole (..), asWitness, hashKey)
import Shelley.Spec.Ledger.LedgerState (KeyPairs)
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.Scripts
  ( MultiSig,
    getKeyCombinations,
    pattern RequireAllOf,
    pattern RequireAnyOf,
    pattern RequireMOf,
    pattern RequireSignature,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (TxId (..), TxIn (..), TxOut (..), ValidateScript (..))
import Shelley.Spec.Ledger.TxBody (TxBody (TxBody, _inputs, _outputs, _txfee), Wdrl (..))
import Shelley.Spec.Ledger.UTxO (UTxO)
import Test.QuickCheck (Gen, choose, shuffle)
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants (..),
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( EraGen (..),
    genCoin,
    genNatural,
    genTxOut,
    genesisCoins,
  )
import Test.Shelley.Spec.Ledger.Generator.MetaData (genMetaData)
import Test.Shelley.Spec.Ledger.Generator.Presets (keyPairs, someKeyPairs)

instance CC.Crypto c => EraGen (ShelleyEra c) where
  genEraUtxo0 = genUtxo0
  genEraTxBody = genTxBody
  genMetadata = genMetaData

  eraScriptWitnesses = getKeyCombinations

  eraKeySpaceScripts = mSigCombinedScripts

  updateEraTxBody body fee ins outs =
    body
      { _txfee = fee,
        _inputs = ins,
        _outputs = outs
      }

-- | Generate a transaction body with the given inputs/outputs and certificates
genTxBody ::
  (ShelleyBased era) =>
  SlotNo ->
  Set (TxIn era) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  Wdrl era ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (MetaDataHash era) ->
  Gen (TxBody era)
genTxBody slot inputs outputs certs wdrls fee update mdHash = do
  ttl <- genTimeToLive slot
  return $
    TxBody
      inputs
      outputs
      certs
      wdrls
      fee
      ttl
      update
      mdHash

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

genUtxo0 ::
  forall era.
  ( ShelleyBased era,
    Core.Script era ~ MultiSig era,
    Core.Value era ~ Coin,
    ValidateScript era
  ) =>
  Constants ->
  Gen (UTxO era)
genUtxo0 c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts, maxGenesisOutputVal, minGenesisOutputVal} = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts @era (mSigCombinedScripts c) minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    genTxOut
      (genCoin minGenesisOutputVal maxGenesisOutputVal)
      (fmap (toAddr Testnet) genesisKeys ++ fmap (scriptsToAddr' Testnet) genesisScripts)
  return (genesisCoins genesisId outs)
  where
    scriptsToAddr' :: Network -> (Core.Script era, Core.Script era) -> Addr era
    scriptsToAddr' n (payScript, stakeScript) =
      Addr n (scriptToCred' payScript) (StakeRefBase $ scriptToCred' stakeScript)
    scriptToCred' = ScriptHashObj . hashScript

genesisId ::
  forall era.
  ( ShelleyBased era,
    Core.Script era ~ MultiSig era
  ) =>
  TxId era
genesisId =
  TxId $
    hashAnnotated
      ( (TxBody @era)
          Set.empty
          StrictSeq.Empty
          StrictSeq.Empty
          (Wdrl Map.empty)
          (Coin 0)
          (SlotNo 0)
          SNothing
          SNothing
      )

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts`.
someScripts ::
  forall era.
  [(MultiSig era, MultiSig era)] ->
  Int ->
  Int ->
  Gen [(MultiSig era, MultiSig era)]
someScripts scripts lower upper =
  take
    <$> choose (lower, upper)
    <*> shuffle scripts

mSigBaseScripts :: Era era => Constants -> [(MultiSig era, MultiSig era)]
mSigBaseScripts c = mkMSigScripts (keyPairs c)

mSigCombinedScripts :: Era era => Constants -> [(MultiSig era, MultiSig era)]
mSigCombinedScripts c@(Constants {numBaseScripts}) =
  mkMSigCombinations . take numBaseScripts $ mSigBaseScripts c

-- | Multi-Sig Scripts based on the given key pairs
mkMSigScripts :: (Era era) => KeyPairs (Crypto era) -> [(MultiSig era, MultiSig era)]
mkMSigScripts = map mkScriptsFromKeyPair

mkScriptsFromKeyPair ::
  (Era era) =>
  (KeyPair 'Payment (Crypto era), KeyPair 'Staking (Crypto era)) ->
  (MultiSig era, MultiSig era)
mkScriptsFromKeyPair (k0, k1) =
  (mkScriptFromKey $ asWitness k0, mkScriptFromKey $ asWitness k1)

mkScriptFromKey :: (Era era) => KeyPair 'Witness (Crypto era) -> MultiSig era
mkScriptFromKey = (RequireSignature . hashKey . vKey)

-- | Combine a list of multisig pairs into hierarchically structured multi-sig
-- scripts, list must have at least length 3. Be careful not to call with too
-- many pairs in order not to create too many of the possible combinations.
mkMSigCombinations ::
  Era era =>
  [(MultiSig era, MultiSig era)] ->
  [(MultiSig era, MultiSig era)]
mkMSigCombinations msigs =
  if length msigs < 3
    then error "length of input msigs must be at least 3"
    else foldl' (++) [] $
      do
        (k1, k2) <- msigs
        (k3, k4) <- msigs List.\\ [(k1, k2)]
        (k5, k6) <- msigs List.\\ [(k1, k2), (k3, k4)]

        pure
          [ (pay, stake)
            | pay <-
                [ RequireAnyOf [k1, k3, k5],
                  RequireAllOf [k1, k3, k5],
                  RequireMOf 1 [k1, k3, k5],
                  RequireMOf 2 [k1, k3, k5],
                  RequireMOf 3 [k1, k3, k5]
                ],
              stake <-
                [ RequireAnyOf [k2, k4, k6],
                  RequireAllOf [k2, k4, k6],
                  RequireMOf 1 [k2, k4, k6],
                  RequireMOf 2 [k2, k4, k6],
                  RequireMOf 3 [k2, k4, k6]
                ]
          ]
