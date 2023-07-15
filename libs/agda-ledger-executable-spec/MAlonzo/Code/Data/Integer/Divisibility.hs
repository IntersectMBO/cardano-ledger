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

module MAlonzo.Code.Data.Integer.Divisibility where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Divisibility
import qualified MAlonzo.Code.Data.Nat.Divisibility.Core
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Double

-- Data.Integer.Divisibility._∣_
d__'8739'__6 :: Integer -> Integer -> ()
d__'8739'__6 = erased
-- Data.Integer.Divisibility.*-monoʳ-∣
d_'42''45'mono'691''45''8739'_12 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_'42''45'mono'691''45''8739'_12 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin__110
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
         (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
      (coe
         MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v2)))
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
         (mulInt
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
         (mulInt
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v2)))
         (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v2)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
               (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
            (coe
               MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
               (coe
                  MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v2))))
         v3)
-- Data.Integer.Divisibility.*-monoˡ-∣
d_'42''45'mono'737''45''8739'_30 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_'42''45'mono'737''45''8739'_30 v0 v1 v2
  = coe d_'42''45'mono'691''45''8739'_12 (coe v0) (coe v1) (coe v2)
-- Data.Integer.Divisibility.*-cancelˡ-∣
d_'42''45'cancel'737''45''8739'_54 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_'42''45'cancel'737''45''8739'_54 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'737''45''8739'_54 v0 v1 v2 v4
du_'42''45'cancel'737''45''8739'_54 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_'42''45'cancel'737''45''8739'_54 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin__110
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
         (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
      (coe
         mulInt
         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v1)))
      (coe
         mulInt
         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
         (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v2)))
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
         (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v1)))
         (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
            (coe
               MAlonzo.Code.Data.Integer.Base.d__'42'__308 (coe v0) (coe v2)))
         (mulInt
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
            (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v2)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
               (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
            (coe
               mulInt
               (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
               (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v2))))
         v3)
-- Data.Integer.Divisibility.*-cancelʳ-∣
d_'42''45'cancel'691''45''8739'_76 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_'42''45'cancel'691''45''8739'_76 v0 v1 v2 ~v3
  = du_'42''45'cancel'691''45''8739'_76 v0 v1 v2
du_'42''45'cancel'691''45''8739'_76 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_'42''45'cancel'691''45''8739'_76 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''8739'_54 (coe v0) (coe v1) (coe v2)
