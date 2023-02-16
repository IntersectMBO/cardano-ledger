{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Protocol.TPraos.Rules where

import Cardano.Ledger.BaseTypes (BlocksMade (BlocksMade), EpochNo, Nonce (NeutralNonce))
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Core (EraTxOut, PParams)
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Keys (
  GenDelegPair (GenDelegPair),
  GenDelegs (GenDelegs),
  HasKeyRole (coerceKeyRole),
  KeyHash,
  KeyRole (BlockIssuer, Genesis),
 )
import Cardano.Ledger.PoolDistr (PoolDistr (PoolDistr))
import Cardano.Ledger.Shelley.Core (EraGovernance (emptyGovernanceState))
import Cardano.Ledger.Shelley.LedgerState (AccountState (AccountState), DPState (DPState), DState (dsGenDelegs), EpochState (EpochState), LedgerState (LedgerState), NewEpochState (NewEpochState), StashedAVVMAddresses, smartUTxOState)
import Cardano.Ledger.UTxO (UTxO)
import Cardano.Protocol.TPraos.BHeader (LastAppliedBlock)
import Cardano.Slotting.Slot (WithOrigin)
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Word (Word64)
import GHC.Generics (Generic)

data CHAIN era

data ChainState era = ChainState
  { chainNes :: NewEpochState era
  , chainOCertIssue :: Map.Map (KeyHash 'BlockIssuer (EraCrypto era)) Word64
  , chainEpochNonce :: Nonce
  , chainEvolvingNonce :: Nonce
  , chainCandidateNonce :: Nonce
  , chainPrevEpochNonce :: Nonce
  , chainLastAppliedBlock :: WithOrigin (LastAppliedBlock (EraCrypto era))
  }
  deriving (Generic)

-- | Creates a valid initial chain state
initialShelleyState ::
  ( EraTxOut era
  , EraGovernance era
  , Default (StashedAVVMAddresses era)
  ) =>
  WithOrigin (LastAppliedBlock (EraCrypto era)) ->
  EpochNo ->
  UTxO era ->
  Coin ->
  Map.Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  PParams era ->
  Nonce ->
  ChainState era
initialShelleyState lab e utxo reserves genDelegs pp initNonce =
  ChainState
    ( NewEpochState
        e
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        ( EpochState
            (AccountState (Coin 0) reserves)
            emptySnapShots
            ( LedgerState
                ( smartUTxOState
                    pp
                    utxo
                    (Coin 0)
                    (Coin 0)
                    emptyGovernanceState
                )
                (DPState (def {dsGenDelegs = GenDelegs genDelegs}) def)
            )
            pp
            pp
            def
        )
        SNothing
        (PoolDistr Map.empty)
        def
    )
    cs
    initNonce
    initNonce
    initNonce
    NeutralNonce
    lab
  where
    cs =
      Map.fromList
        ( fmap
            (\(GenDelegPair hk _) -> (coerceKeyRole hk, 0))
            (Map.elems genDelegs)
        )
