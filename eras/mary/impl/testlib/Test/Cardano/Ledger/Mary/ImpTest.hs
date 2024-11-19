{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ImpTest (
  MaryEraImp,
  module Test.Cardano.Ledger.Allegra.ImpTest,
  mkTokenMintingTx,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.TreeDiff ()

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , ADDRHASH c ~ Blake2b_224
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (MaryEra c)
  where
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = shelleyFixupTx

class
  ( ShelleyEraImp era
  , MaryEraTxBody era
  , NativeScript era ~ Timelock era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  MaryEraImp era

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , ADDRHASH c ~ Blake2b_224
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  MaryEraImp (MaryEra c)

mkTokenMintingTx :: MaryEraImp era => ScriptHash (EraCrypto era) -> ImpTestM era (Tx era)
mkTokenMintingTx sh = do
  name <- arbitrary
  count <- choose (1, 10)
  let policyId = PolicyID sh
  let ma = multiAssetFromList [(policyId, name, count)]
  addr <- freshKeyAddr_
  pure $
    mkBasicTx mkBasicTxBody
      & bodyTxL . mintTxBodyL .~ ma
      & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr (MaryValue mempty ma)]
