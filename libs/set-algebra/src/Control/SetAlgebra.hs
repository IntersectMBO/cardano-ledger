{-# LANGUAGE GADTs #-}

module Control.SetAlgebra
  ( -- In addition to Data.Map.Map and Data.Set.Set, the following new types can be used in the set algegra
    List, -- A list type whose constructor is hidden (sorted [(key,value)] pairs, with no duplicate keys).
    -- Use 'fromList' to constuct concrete values
    BiMap,
    Bimap, -- Maps for Bijections. Use 'biMapFromList' and 'biMapEmpty' toconstruct concrete values.
    Single (..), -- Sets with a single pair. Visible constructors 'Singleton', 'SetSingleton', and 'Fail'.

    -- Classes supporting abstract constructors of Set Algebra Expressions. These show up in the types of overloaded functions.
    Basic (..),
    Iter (..),
    Embed (..),
    HasExp (..),
    BaseRep (..),
    -- Overloaded functions acting as abstract constructors of Set Algebra Expressions. These correspond
    -- with the operators in the specification, except here sets are thought of as a map with a Unit value. (Map k ())
    dom,
    rng,
    dexclude,
    drestrict,
    rexclude,
    rrestrict,
    unionleft,
    unionright,
    unionplus,
    singleton,
    setSingleton,
    intersect,
    subset,
    keyeq,
    (◁),
    (⋪),
    (▷),
    (⋫),
    (∈),
    (∉),
    (∪),
    (⨃),
    (∪+),
    (∩),
    (⊆),
    (≍),
    (<|),
    (|>),
    (➖),
    -- The only exported concrete Constructor of Set Algebra Expressons. Needed to make 'HasExp' and 'Embed'
    -- instances of new kinds of sets (Basically,  Data.Map's wrapped in a newtype).
    -- See: Cardano.Ledger.Shelley.TxBody and Cardano.Ledger.Shelley.UTxO and
    -- Cardano.Ledger.Shelley.Delegation.Certificates
    -- for example uses of this.
    Exp (Base),
    -- Evaluate an abstract Set Algebra Expression to the Set (Map) it represents.
    eval,
    -- Functions to build concrete Set-like things useable as Set Algebra Expressions
    materialize,
    biMapToMap,
    biMapFromMap,
    biMapFromList,
    biMapEmpty,
    fromList,
    keysEqual,
    forwards,
    backwards,
  )
where

import Control.Iterate.BaseTypes (BaseRep (..), Basic (..), Embed (..), Iter (..), List, Single (..))
import Control.Iterate.Exp
  ( Exp (..),
    HasExp (..),
    dexclude,
    dom,
    drestrict,
    intersect,
    keyeq,
    rexclude,
    rng,
    rrestrict,
    setSingleton,
    singleton,
    subset,
    unionleft,
    unionplus,
    unionright,
    (<|),
    (|>),
    (∈),
    (∉),
    (∩),
    (∪),
    (∪+),
    (≍),
    (⊆),
    (⋪),
    (⋫),
    (▷),
    (◁),
    (➖),
    (⨃),
  )
import Control.Iterate.SetAlgebra
import Data.BiMap (BiMap (..), Bimap, biMapEmpty, biMapFromList, biMapFromMap, biMapToMap)
import Data.Map (Map)
import Data.MapExtras (keysEqual)
import Data.Set (Set)

forwards :: BiMap v k v -> Map k v
forwards (MkBiMap l _r) = l

backwards :: BiMap v k v -> Map v (Set k)
backwards (MkBiMap _l r) = r
