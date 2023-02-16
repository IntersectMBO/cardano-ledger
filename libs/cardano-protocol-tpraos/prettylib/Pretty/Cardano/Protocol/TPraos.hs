{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pretty.Cardano.Protocol.TPraos where

import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Pretty (PDoc, PrettyA (prettyA), ppBlockNo, ppHash, ppKeyHash, ppNatural, ppProtVer, ppRecord, ppSet, ppSexp, ppSignedDSIGN, ppSlotNo, ppString, ppVKey, ppVerKeyKES, text)
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
  BHeader (BHeader),
  HashHeader (..),
  LastAppliedBlock (..),
  PrevHash (..),
 )
import Cardano.Protocol.TPraos.OCert (
  KESPeriod (..),
  OCert (..),
  OCertEnv (..),
  OCertSignable (..),
 )
import Data.Proxy (Proxy (Proxy))
import Prettyprinter (pretty, viaShow, (<+>))

ppLastAppliedBlock :: LastAppliedBlock c -> PDoc
ppLastAppliedBlock (LastAppliedBlock blkNo slotNo hh) =
  ppRecord
    "LastAppliedBlock"
    [ ("blockNo", ppBlockNo blkNo)
    , ("slotNo", ppSlotNo slotNo)
    , ("hash", ppHashHeader hh)
    ]

ppHashHeader :: HashHeader c -> PDoc
ppHashHeader (HashHeader x) = ppHash x

instance PrettyA (LastAppliedBlock c) where
  prettyA = ppLastAppliedBlock

instance PrettyA (HashHeader c) where
  prettyA = ppHashHeader

ppBHBody :: Crypto c => BHBody c -> PDoc
ppBHBody (BHBody bn sn prev vk vrfvk eta l size hash ocert protver) =
  ppRecord
    "BHBody"
    [ ("BlockNo", ppBlockNo bn)
    , ("SlotNo", ppSlotNo sn)
    , ("Prev", ppPrevHash prev)
    , ("VKey", ppVKey vk)
    , ("VerKeyVRF", viaShow vrfvk) -- The next 3 are type families
    , ("Eta", viaShow eta)
    , ("L", viaShow l)
    , ("size", ppNatural size)
    , ("Hash", ppHash hash)
    , ("OCert", ppOCert ocert)
    , ("ProtVersion", ppProtVer protver)
    ]

ppPrevHash :: PrevHash c -> PDoc
ppPrevHash GenesisHash = ppString "GenesisHash"
ppPrevHash (BlockHash x) = ppSexp "BlockHashppHashHeader" [ppHashHeader x]

ppBHeader :: Crypto c => BHeader c -> PDoc
ppBHeader (BHeader bh sig) =
  ppRecord
    "BHeader"
    [ ("Body", ppBHBody bh)
    , ("Sig", viaShow sig)
    ]

instance Crypto c => PrettyA (BHBody c) where
  prettyA = ppBHBody

instance Crypto c => PrettyA (BHeader c) where
  prettyA = ppBHeader

instance PrettyA (PrevHash c) where
  prettyA = ppPrevHash

ppKESPeriod :: KESPeriod -> PDoc
ppKESPeriod (KESPeriod x) = text "KESPeriod" <+> pretty x

ppOCertEnv :: OCertEnv c -> PDoc
ppOCertEnv (OCertEnv ps ds) =
  ppRecord
    "OCertEnv"
    [ ("ocertEnvStPools", ppSet ppKeyHash ps)
    , ("ocertEnvGenDelegs", ppSet ppKeyHash ds)
    ]

ppOCert :: forall c. Crypto c => OCert c -> PDoc
ppOCert (OCert vk n per sig) =
  ppRecord
    "OCert"
    [ ("ocertVkKot", ppVerKeyKES (Proxy @c) vk)
    , ("ocertN", pretty n)
    , ("ocertKESPeriod", ppKESPeriod per)
    , ("ocertSigma", ppSignedDSIGN sig)
    ]

ppOCertSignable :: forall c. Crypto c => OCertSignable c -> PDoc
ppOCertSignable (OCertSignable verkes w per) =
  ppSexp "OCertSignable" [ppVerKeyKES (Proxy @c) verkes, pretty w, ppKESPeriod per]

instance PrettyA KESPeriod where
  prettyA = ppKESPeriod

instance PrettyA (OCertEnv c) where
  prettyA = ppOCertEnv

instance Crypto c => PrettyA (OCert c) where
  prettyA = ppOCert

instance Crypto c => PrettyA (OCertSignable c) where
  prettyA = ppOCertSignable
