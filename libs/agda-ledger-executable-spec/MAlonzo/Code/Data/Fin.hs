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

module MAlonzo.Code.Data.Fin where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Data.Bool.Properties
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.Fin.#_
d_'35'__10 ::
  Integer ->
  Integer -> AgdaAny -> MAlonzo.Code.Data.Fin.Base.T_Fin_10
d_'35'__10 v0 v1 ~v2 = du_'35'__10 v0 v1
du_'35'__10 ::
  Integer -> Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10
du_'35'__10 v0 v1
  = coe
      MAlonzo.Code.Data.Fin.Base.du_fromℕ'60'_52 (coe v0)
      (coe
         MAlonzo.Code.Relation.Nullary.Decidable.Core.du_toWitness_108
         (coe
            MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
            (\ v2 ->
               coe
                 MAlonzo.Code.Data.Nat.Properties.du_'8804''7495''8658''8804'_2536
                 (coe addInt (coe (1 :: Integer)) (coe v0)))
            (coe
               MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
               (coe ltInt (coe v0) (coe v1)))))
