module Cardano.Ledger.Crypto
  {-# DEPRECATED
    "@Cardano.Ledger.Crypto@ interface have been completely overhauled. In particular: \
    \\
    \1). `Crypto` type class no longer contains `DSIGN`, `HASH` and `ADDRHASH` type families, \
    \  instead they have been extracted into type synonyms that point to exact algorithms that previously \
    \  where specified in `StandardCrypto` for those type families: \
    \    a). `DSIGN` type synonym can be imported from \"Cardano.Ledger.Keys\" module \
    \    b). `HASH` and `ADDRHASH` type synonyms can be imported from \"Cardano.Ledger.Hashes\" module \
    \\
    \2). `Crypto` type class has retained its `KES` and `VRF` type families, which are not used in Ledger, therefore \
    \  this type class as well as `StandardCrypto` definition will be migrated to \"Cardano.Protocol.Crypto\" module \
    \  and from now on should be imported from 'cardano-protocol-tpraos' package instead."
    #-} (
  Crypto (..),
  StandardCrypto,
  HASH,
  ADDRHASH,
  DSIGN,
) where

import Cardano.Ledger.Crypto.Internal
