{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Scripts (
  module Cardano.Ledger.Allegra.Scripts,
)
where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)

type instance SomeScript 'PhaseOne (MaryEra c) = Timelock (MaryEra c)

-- | Since Timelock scripts are a strictly backwards compatible extension of
-- MultiSig scripts, we can use the same 'scriptPrefixTag' tag here as we did
-- for the ValidateScript instance in MultiSig
instance Crypto c => EraScript (MaryEra c) where
  type Script (MaryEra c) = Timelock (MaryEra c)
  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"
  phaseScript PhaseOneRep timelock = Just (Phase1Script timelock)
  phaseScript PhaseTwoRep _ = Nothing
