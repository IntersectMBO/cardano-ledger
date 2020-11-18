{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternGuards #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- =============================================================================

module Test.Shelley.Spec.Ledger.Generator.GenEra
  ( ScriptClass(..),
    Quantifier(..),
    anyOf, allOf, mOf,
    EraGen(),
    proxy,
  )
  where

import Shelley.Spec.Ledger.Scripts(MultiSig(..),ScriptHash,getKeyCombination,hashMultiSigScript)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Core(Script,Value,TxBody,ChainData, SerialisableData, AnnotatedData)
import Cardano.Ledger.Era (Era(..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Shelley.Spec.Ledger.Keys (Hash, KeyHash, KeyRole (..),VKey(..), hashKey, asWitness)
import Cardano.Binary(ToCBOR(..),FromCBOR(..),Annotator)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Compactible(Compactible(..),CompactForm(..))
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Hashing (EraIndependentTxBody, HashAnnotated (..))
import Data.Proxy
import Cardano.Ledger.Val(Val(..))
import qualified Cardano.Crypto.Hash as Hash
import Shelley.Spec.Ledger.TxBody() -- import instances only
import Data.List(concatMap,permutations)
import Test.Shelley.Spec.Ledger.Generator.Scripts

-- ==============================================================================

{-
data Field t era where
  Field :: (a -> t -> t) -> (EraGen era => Gen a) -> Field t era
-}

proxy :: forall t. Proxy t
proxy = Proxy

class (Era era,

       ScriptClass era,
       ToCBOR(Core.Script era),
       FromCBOR(Annotator (Core.Script era)),
       Eq (Core.Script era),
       Show (Core.Script era),
       NoThunks (Core.Script era),

       Eq (Core.TxBody era),
       Show (Core.TxBody era),
       HashIndex (Core.TxBody era) ~ EraIndependentTxBody,
       ToCBOR (Core.TxBody era),
       FromCBOR(Annotator (Core.TxBody era)),
       HashAnnotated(Core.TxBody era) era,
       NoThunks (Core.TxBody era),


       Val (Core.Value era),
       Eq (Core.Value era),
       Show (Core.Value era),
       NoThunks (Core.Value era),
       Compactible (Core.Value era),
       Torsor (Core.Value era),
       ToCBOR (Core.Value era),
       ToCBOR(Delta (Core.Value era)),
       ToCBOR(CompactForm (Core.Value era)),
       FromCBOR(Core.Value era),
       FromCBOR(Delta (Core.Value era)),
       FromCBOR(CompactForm (Core.Value era))

      ) => EraGen era where

-- ==========================================================
-- The (ShelletEra c) instance of ScriptClass and EraGen

instance CC.Crypto c => EraGen (ShelleyEra c) where
