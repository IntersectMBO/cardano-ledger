{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SerialiseLedger (
  serialNES,
  deSerialNES,
  encode,
  decode,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  Decoder,
  EncCBOR (..),
  Encoding,
  Interns (..),
  Iso (..),
  decNoShareCBOR,
  decShareMapWithIso,
  decodeMap,
  encodeMap,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Density (..),
  Encode (..),
  Wrapped (..),
  decode,
  encode,
  (!>),
  (<!),
  (<!>),
 )
import Cardano.Ledger.Core (Era (..), EraTxOut (..), TxOut)

-- import Cardano.Ledger.Crypto(Crypto)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.EpochBoundary ()
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), coerceKeyRole)
import Cardano.Ledger.Shelley.Governance (EraGov (..), GovState)
import Cardano.Ledger.Shelley.LedgerState

-- SnapShots,SnapShot)
import Cardano.Ledger.UTxO (UTxO (..))

-- import Control.Monad.Trans(lift)
import Lens.Micro (Lens', lens, _1, _2)

-- import qualified Data.Map as Map
-- import Data.Typeable(Typeable)
-- import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)
-- import Data.Map.Strict.Internal (Map (..))

idL :: Lens' x x
idL = lens getter setter
  where
    getter x = x
    setter _ x = x

-- =============================================

-- | An Encode function for NewEpochState that does something different with the TxOut in the UTxO
--   (encode (serialNES encodeTxOut nes)) can be used just like (encCBOR nes)
serialNES ::
  ( Era era
  , EncCBOR (GovState era)
  , EncCBOR (StashedAVVMAddresses era)
  ) =>
  (TxOut era -> Encoding) ->
  NewEpochState era ->
  Encode ('Closed 'Dense) (NewEpochState era)
serialNES encTxOut (NewEpochState e bp bc es ru pd av) =
  ( Rec NewEpochState
      !> To e
      !> To bp
      !> To bc
      !> ( let EpochState acct ls snap myop = es
            in ( Rec EpochState
                  !> To acct
                  !> ( let LedgerState utxo cert = ls
                        in ( Rec (\certS uS -> LedgerState uS certS)
                              !> To cert -- certstate first to improve sharing
                              !> ( let UTxOState (UTxO u) dp fs us sd don = utxo
                                    in ( Rec (\ut -> UTxOState (UTxO ut))
                                          !> E (encodeMap encCBOR encTxOut) u
                                          !> To dp
                                          !> To fs
                                          !> To us
                                          !> To sd
                                          !> To don
                                       )
                                 )
                           )
                     )
                  !> To snap
                  !> To myop
               )
         )
      !> To ru
      !> To pd
      !> To av
  )

-- | An Decode function for NewEpochState that does something different with the TxOut in the UTxO
--   It can Decode what 'serialNES' encodes, and can use more efficient algorithms on TxOut
--   in particular how it handles the compact form of MultiAsset.
deSerialNES ::
  ( Era era
  , DecShareCBOR (GovState era)
  , DecCBOR (StashedAVVMAddresses era)
  ) =>
  (forall s. Decoder s (TxOut era)) ->
  Decode ('Closed 'Dense) (NewEpochState era)
deSerialNES decTxOut =
  ( RecD NewEpochState
      <! From
      <! From
      <! From
      <! ( RecD EpochState
            <! From
            <! ( RecD (\cert utxo -> LedgerState utxo cert)
                  <! D decNoShareCBOR -- CertState
                  <! ( RecD (\u -> UTxOState (UTxO u)) --  Interns (Credential 'Staking (EraCrypto era))
                        <! D (decodeMap decCBOR decTxOut) -- Interns (Credential 'Staking (EraCrypto era))
                        <! From
                        <! From
                        <! D decNoShareCBOR -- GovState () Probaly different for Conway Era
                        <! D decNoShareCBOR -- IncementalStake (Interns (Credential 'Staking c))
                        <! From
                     )
               )
            <! From
            <! D decNoShareCBOR -- NonMyopic (Interns (KeyHash 'StakePool c))
         )
      <! From
      <! From
      <! From
  )

instance
  ( Era era
  , EraTxOut era
  , EraGov era
  , StashedAVVMAddresses era ~ ()
  ) =>
  DecShareCBOR (NewEpochState era)
  where
  type
    Share (NewEpochState era) =
      (Interns (Credential 'Staking (EraCrypto era)), Interns (KeyHash 'StakePool (EraCrypto era)))
  decShareCBOR share@(_cred, _khash) =
    decode $
      RunShare share $
        ( Pure (RecD NewEpochState)
            <!> Pure From -- EpochNo
            <!> Pure From -- prevBlocks
            <!> Pure From -- currBlocks
            <!> ( Pure (RecD EpochState)
                    <!> Pure From
                    <!> ( Pure (RecD (\cert utxo -> LedgerState utxo cert))
                            <!> WriteShare idL -- CertState (Credential 'Staking c,KeyHash 'StakePool c)
                            <!> ( Pure (RecD UTxOState) -- WriteShare _1 -- UTxOState
                                    <!> WriteShare _1 -- UTxO   (Credential 'Staking c)
                                    <!> Pure From -- deposited
                                    <!> Pure From -- fees
                                    <!> Pure From -- WriteShare idL    --
                                    -- govState
                                    <!> WriteShare _1 -- stakeDistr (Credential 'Staking c)
                                    <!> Pure From -- donation
                                )
                        )
                    <!> WriteShare idL -- Snapshots
                    <!> WriteShare _2 -- NonMyopic (KeyHash 'StakePool c)
                )
            <!> Pure From -- pulser
            <!> Pure From -- poolDistr
            <!> Pure From -- stashed
        )

newtype VState' era = VState' (VState era)

instance Era era => DecShareCBOR (VState' era) where
  type Share (VState' era) = (Interns (Credential 'Staking (EraCrypto era)))
  decShareCBOR interns =
    decode $
      RunShare interns $
        ( Pure (RecD (\x y z -> VState' (VState x y z)))
            <!> Emit (decShareMapWithIso credIso) -- credIso :: Iso (Credential 'Staking c) (Credential Voting c)
            <!> Pure From
            <!> Pure From
        )

credIso :: Iso (Credential 'Staking c) (Credential kr c)
credIso = Iso coerceKeyRole coerceKeyRole

{-
:i Share
type DecShareCBOR :: * -> Constraint
class Monoid (Share a) => DecShareCBOR a where
  type Share :: * -> *
  type family Share a
    Default: ()
  ...
        -- Defined in ‘cardano-ledger-binary-1.3.1.0:Cardano.Ledger.Binary.Decoding.Sharing’
type instance Share (InstantaneousRewards c)
  = Interns (Credential 'Staking c)
        -- Defined in ‘Cardano.Ledger.CertState’
type instance Share (DState era)
  = (Interns (Credential 'Staking (EraCrypto era)),
     Interns (KeyHash 'StakePool (EraCrypto era)))
        -- Defined in ‘Cardano.Ledger.CertState’
type instance Share (PState era)
  = Interns (KeyHash 'StakePool (EraCrypto era))
        -- Defined in ‘Cardano.Ledger.CertState’
type instance Share (VState era) = ()
        -- Defined in ‘Cardano.Ledger.CertState’
type instance Share (CertState era)
  = (Interns (Credential 'Staking (EraCrypto era)),
     Interns (KeyHash 'StakePool (EraCrypto era)))
        -- Defined in ‘Cardano.Ledger.CertState’
type instance Share (IncrementalStake c)
  = Interns (Credential 'Staking c)
        -- Defined in ‘cardano-ledger-shelley-1.10.0.0:Cardano.Ledger.Shelley.LedgerState.Types’
type instance Share (UTxOState era)
  = Interns (Credential 'Staking (EraCrypto era))
        -- Defined in ‘cardano-ledger-shelley-1.10.0.0:Cardano.Ledger.Shelley.LedgerState.Types’
type instance Share (LedgerState era)
  = (Interns (Credential 'Staking (EraCrypto era)),
     Interns (KeyHash 'StakePool (EraCrypto era)))
        -- Defined in ‘cardano-ledger-shelley-1.10.0.0:Cardano.Ledger.Shelley.LedgerState.Types’
type instance Share (UTxO era)
  = Interns (Credential 'Staking (EraCrypto era))
        -- Defined in ‘Cardano.Ledger.UTxO’
type instance Share (ShelleyGovState era) = ()
        -- Defined in ‘Cardano.Ledger.Shelley.Governance’

-}
