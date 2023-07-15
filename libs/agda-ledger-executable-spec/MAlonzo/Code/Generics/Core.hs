{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Generics.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text

-- Generics.Core.PatTelescope
d_PatTelescope_260 :: ()
d_PatTelescope_260 = erased
-- Generics.Core.Context
d_Context_262 :: ()
d_Context_262 = erased
-- Generics.Core.TTerm
d_TTerm_264 :: ()
d_TTerm_264 = erased
-- Generics.Core.Hole
d_Hole_266 :: ()
d_Hole_266 = erased
-- Generics.Core.THole
d_THole_268 :: ()
d_THole_268 = erased
-- Generics.Core.Tactic
d_Tactic_270 :: ()
d_Tactic_270 = erased
-- Generics.Core.UnquoteDecl
d_UnquoteDecl_272 :: ()
d_UnquoteDecl_272 = erased
