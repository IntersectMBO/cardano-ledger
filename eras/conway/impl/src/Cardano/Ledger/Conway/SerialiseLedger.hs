{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Conway.SerialiseLedger (
  serialNES,
  deSerialNES,
  encode,
  decode,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR,
  Decoder,
  EncCBOR (..),
  Encoding,
  decNoShareCBOR,
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
 )
import Cardano.Ledger.Core (Era, TxOut)
import Cardano.Ledger.Shelley.Governance (GovState)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UTxO (UTxO (..))

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
                  <! ( RecD (\u -> UTxOState (UTxO u))
                        <! D (decodeMap decCBOR decTxOut)
                        <! From
                        <! From
                        <! D decNoShareCBOR -- GovState
                        <! D decNoShareCBOR -- IncementalStake
                        <! From
                     )
               )
            <! From
            <! D decNoShareCBOR -- NonMyopic
         )
      <! From
      <! From
      <! From
  )
