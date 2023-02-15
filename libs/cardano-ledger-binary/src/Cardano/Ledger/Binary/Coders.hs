-- | "Cardano.Ledger.Binary.Coders" provides tools for writing 'EncCBOR' and 'DecCBOR'
--   instances (see module 'Cardano.Ledger.Binary') in an intuitive way that mirrors the way one
--   constructs values of a particular type. Advantages include:
--
-- 1. Book-keeping details neccesary to write correct instances are hidden from the user.
-- 2. Inverse 'EncCBOR' and 'DecCBOR' instances have visually similar definitions.
-- 3. Advanced instances involving sparse-encoding, compact-representation, and
-- 'Annotator' instances are also supported.
--
-- A Guide to Visual inspection of Duality in Encode and Decode
--
-- 1. @(Sum c)@     and @(SumD c)@    are duals
-- 2. @(Rec c)@     and @(RecD c)@    are duals
-- 3. @(Keyed c)@   and @(KeyedD c)@  are duals
-- 4. @(OmitC x)@   and @(Emit x)@    are duals
-- 5. @(Omit p ..)@ and @(Emit x)@    are duals if (p x) is True
-- 6. @(To x)@      and @(From)@      are duals if (x::T) and (forall (y::T). isRight (roundTrip y))
-- 7. @(E enc x)@   and @(D dec)@     are duals if (forall x . isRight (roundTrip' enc dec x))
-- 8. @(f !> x)@    and @(g <! y)@    are duals if (f and g are duals) and (x and y are duals)
--
-- Duality properties of @(Summands name decodeT)@ and @(SparseKeyed name (init::T) pick
-- required)@ also exist but are harder to describe succinctly.
module Cardano.Ledger.Binary.Coders (
  module Cardano.Ledger.Binary.Encoding.Coders,
  module Cardano.Ledger.Binary.Decoding.Coders,
)
where

import Cardano.Ledger.Binary.Decoding.Coders
import Cardano.Ledger.Binary.Encoding.Coders
