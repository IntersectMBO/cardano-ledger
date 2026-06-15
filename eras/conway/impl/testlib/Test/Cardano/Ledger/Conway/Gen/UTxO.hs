{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Gen.UTxO where

import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (Datum (..))
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript)
import Control.Monad (foldM)
import Control.Monad.State.Strict (MonadState)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Conway.Gen (freshCredential)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Era (nativeAlwaysSucceeds)
import Test.Cardano.Ledger.Shelley.ImpTest (HasKeyPairs)

genScript :: (MonadGen m, ShelleyEraScript era) => m (Script era)
genScript = pure nativeAlwaysSucceeds

genRefScript :: (MonadGen m, ShelleyEraScript era) => m (StrictMaybe (Script era))
genRefScript =
  frequency
    [ (95, pure SNothing)
    , (5, SJust <$> genScript)
    ]

genDatum :: (MonadGen m, Era era) => m (Datum era)
genDatum =
  frequency
    [ (1, pure NoDatum)
    , (1, DatumHash <$> arbitrary)
    , (1, Datum <$> liftGen (scale (`min` 5) arbitrary))
    ]

genCoin :: MonadGen m => m Coin
genCoin =
  Coin
    <$> frequency
      [ (50, choose (2_000_000, 100_000_000))
      , (35, choose (100_000_000, 10_000_000_000))
      , (12, choose (10_000_000_000, 1_000_000_000_000))
      , (3, choose (1_000_000_000_000, 100_000_000_000_000))
      ]

genValue :: MonadGen m => m MaryValue
genValue =
  frequency
    [ (85, MaryValue <$> genCoin <*> pure mempty)
    , (15, MaryValue <$> genCoin <*> arbitrary)
    ]

genAddress ::
  ( MonadGen m
  , HasStatefulGen g m
  , HasKeyPairs s
  , MonadState s m
  , EraAccounts era
  ) =>
  Accounts era ->
  Set Addr ->
  m Addr
genAddress accounts existingAddrs =
  frequency
    [ (2, genFreshAddress)
    , (98, uniformSetElem existingAddrs >>= maybe genFreshAddress pure)
    ]
  where
    genFreshAddress =
      frequency
        [ (98, Addr Testnet <$> freshCredential <*> genStakeRef)
        , (2, AddrBootstrap <$> arbitrary)
        ]
    genStakeRef =
      let accountsMap = accounts ^. accountsMapL
       in frequency
            [
              ( 95
              , StakeRefBase
                  <$> (uniformMapElem accountsMap >>= maybe freshCredential (pure . fst))
              )
            , (1, StakeRefPtr <$> arbitrary)
            , (4, pure StakeRefNull)
            ]

genUTxO ::
  forall era m s g.
  ( MonadGen m
  , HasStatefulGen g m
  , HasKeyPairs s
  , MonadState s m
  , EraAccounts era
  , Accounts era ~ ConwayAccounts era
  , TxOut era ~ BabbageTxOut era
  , ShelleyEraScript era
  , Value era ~ MaryValue
  ) =>
  ConwayAccounts era ->
  Word ->
  m (UTxO era)
genUTxO accounts nUTxO = do
  (_, utxoMap) <- foldM step (Set.empty, Map.empty) [1 .. nUTxO]
  pure (UTxO utxoMap)
  where
    step (addrs, utxoMap) _ = do
      txin <- freshTxIn utxoMap
      address <- genAddress accounts addrs
      val <- genValue
      datum <- genDatum
      refScript <- genRefScript
      pure
        ( Set.insert address addrs
        , Map.insert txin (BabbageTxOut address val datum refScript) utxoMap
        )
    freshTxIn currentMap = do
      txin <- arbitrary
      if Map.member txin currentMap
        then freshTxIn currentMap
        else pure txin
