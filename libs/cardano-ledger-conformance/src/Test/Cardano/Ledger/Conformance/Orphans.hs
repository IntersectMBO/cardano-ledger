{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans where

import Data.Bifunctor (Bifunctor (..))
import Data.Default.Class (Default)
import Data.List (sortOn)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lib
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (FixupSpecRep (..), OpaqueErrorString)
import Test.Cardano.Ledger.Conformance.Utils
import Test.Cardano.Ledger.Conway.TreeDiff (Expr (..), ToExpr (..))

deriving instance Generic (HSSet a)

deriving instance Generic GovActionState

deriving instance Generic Vote

deriving instance Generic GovProposal

deriving instance Generic GovAction

deriving instance Generic GovVote

deriving instance Generic GovSignal

deriving instance Generic GovEnv

deriving instance Generic EnactState

deriving instance Generic CertEnv

deriving instance Generic PState

deriving instance Generic DState

deriving instance Generic GState

deriving instance Generic CertState

deriving instance Generic RatifyEnv

deriving instance Generic RatifyState

deriving instance Generic StakeDistrs

deriving instance Ord Tag

deriving instance Ord Credential

deriving instance Ord GovRole

deriving instance Ord VDeleg

deriving instance Ord Vote

deriving instance Ord GovAction

deriving instance Ord GovActionState

deriving instance Eq a => Eq (HSSet a)

deriving instance Eq AgdaEmpty

deriving instance Eq TxBody

deriving instance Eq Tag

deriving instance Eq TxWitnesses

deriving instance Eq Tx

deriving instance Eq PParams

deriving instance Eq UTxOState

deriving instance Eq GovAction

deriving instance Eq GovVote

deriving instance Eq GovSignal

deriving instance Eq GovProposal

deriving instance Eq Vote

deriving instance Eq GovActionState

deriving instance Eq GovEnv

deriving instance Eq EnactState

deriving instance Eq UTxOEnv

deriving instance Eq CertEnv

deriving instance Eq DState

deriving instance Eq PState

deriving instance Eq GState

deriving instance Eq CertState

deriving instance Eq RatifyState

instance (NFData k, NFData v) => NFData (HSMap k v)

instance NFData a => NFData (HSSet a)

instance NFData GovAction

instance NFData TxId

instance NFData UTxOState

instance NFData Vote

instance NFData Credential

instance NFData GovRole

instance NFData GovActionState

instance NFData AgdaEmpty

instance NFData GovVote

instance NFData GovProposal

instance NFData GovSignal

instance NFData PParams

instance NFData EnactState

instance NFData GovEnv

instance NFData VDeleg

instance NFData TxCert

instance NFData TxBody

instance NFData Tag

instance NFData TxWitnesses

instance NFData Tx

instance NFData UTxOEnv

instance NFData CertEnv

instance NFData PState

instance NFData DState

instance NFData GState

instance NFData CertState

instance NFData StakeDistrs

instance NFData RatifyEnv

instance NFData RatifyState

instance ToExpr a => ToExpr (HSSet a)

instance ToExpr Credential where
  toExpr (KeyHashObj h) = App "KeyHashObj" [agdaHashToExpr 28 h]
  toExpr (ScriptObj h) = App "ScriptObj" [agdaHashToExpr 28 h]

instance (ToExpr k, ToExpr v) => ToExpr (HSMap k v)

instance ToExpr GovAction

instance ToExpr GovRole

instance ToExpr Vote

instance ToExpr TxId where
  toExpr (MkTxId x) = App "TxId" [agdaHashToExpr 32 x]

instance ToExpr GovActionState

instance ToExpr GovProposal

instance ToExpr GovVote

instance ToExpr GovSignal

instance ToExpr PParams

instance ToExpr GovEnv

instance ToExpr EnactState

instance ToExpr VDeleg

instance ToExpr TxCert

instance ToExpr TxBody

instance ToExpr AgdaEmpty

instance ToExpr Tag

instance ToExpr TxWitnesses

instance ToExpr Tx

instance ToExpr UTxOState

instance ToExpr UTxOEnv

instance ToExpr CertEnv

instance ToExpr DState

instance ToExpr PState

instance ToExpr GState

instance ToExpr CertState

instance ToExpr StakeDistrs

instance ToExpr RatifyEnv

instance ToExpr RatifyState

instance Default (HSMap k v)

instance FixupSpecRep OpaqueErrorString

instance FixupSpecRep a => FixupSpecRep [a]

instance FixupSpecRep Char where
  fixup = id

instance
  ( Ord k
  , FixupSpecRep k
  , FixupSpecRep v
  ) =>
  FixupSpecRep (HSMap k v)
  where
  fixup (MkHSMap l) = MkHSMap . sortOn fst $ bimap fixup fixup <$> l

instance (Ord a, FixupSpecRep a) => FixupSpecRep (HSSet a) where
  fixup (MkHSSet l) = MkHSSet . Set.toList . Set.fromList $ fixup <$> l

instance (FixupSpecRep a, FixupSpecRep b) => FixupSpecRep (a, b)

instance FixupSpecRep a => FixupSpecRep (Maybe a)

instance (FixupSpecRep a, FixupSpecRep b) => FixupSpecRep (Either a b)

instance FixupSpecRep Integer where
  fixup = id

instance FixupSpecRep Bool

instance FixupSpecRep TxId

instance FixupSpecRep ()

instance FixupSpecRep UTxOState

instance FixupSpecRep Credential

instance FixupSpecRep GovRole

instance FixupSpecRep VDeleg

instance FixupSpecRep DState

instance FixupSpecRep PState

instance FixupSpecRep GState

instance FixupSpecRep CertState

instance FixupSpecRep Vote

instance FixupSpecRep GovAction

instance FixupSpecRep GovActionState

instance FixupSpecRep AgdaEmpty

instance FixupSpecRep StakeDistrs

instance FixupSpecRep PParams

instance FixupSpecRep EnactState

instance FixupSpecRep RatifyEnv

instance FixupSpecRep RatifyState
