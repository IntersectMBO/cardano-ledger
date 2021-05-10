{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Elaborators.Shelley where

import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (KES, DSIGN)
import Cardano.Ledger.Shelley (ShelleyEra)
import Data.Maybe.Strict (StrictMaybe(..))
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError(..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.STS.EraMapping ()
import Shelley.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoPredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure(..))
import Test.Cardano.Ledger.ModelChain
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Ledger.Val as Val
import qualified Shelley.Spec.Ledger.Tx as Shelley

instance
    ( PraosCrypto crypto
    , KES.Signable (KES crypto) ~ SignableRepresentation
    , DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
    ) => ElaborateEraModel (ShelleyEra crypto) where

  toEraPredicateFailure = \case
    ModelValueNotConservedUTxO x y -> ApplyBlockTransitionError_Tx $ ApplyTxError
      [UtxowFailure (UtxoFailure (ValueNotConservedUTxO (Val.inject $ Coin $ unModelValue x) (Val.inject $ Coin $ unModelValue y)))]

  makeTxBody _ maxTTL fee ins outs dcerts wdrl = Shelley.TxBody
    { Shelley._inputs = ins
    , Shelley._outputs = outs
    , Shelley._certs = dcerts
    , Shelley._wdrls = wdrl
    , Shelley._txfee = fee
    , Shelley._ttl = maxTTL
    , Shelley._txUpdate = SNothing
    , Shelley._mdHash = SNothing
    }

  makeTx _ realTxBody wits
    = Shelley.Tx realTxBody (mempty {Shelley.addrWits = wits}) SNothing
