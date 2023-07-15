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

module MAlonzo.Code.Data.List.Relation.Unary.Unique.DecSetoid.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.All.Properties
import qualified MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core
import qualified MAlonzo.Code.Data.List.Relation.Unary.Unique.Setoid.Properties
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.List.Relation.Unary.Unique.DecSetoid.Properties._._.AllPairs
d_AllPairs_60 a0 a1 a2 a3 = ()
-- Data.List.Relation.Unary.Unique.DecSetoid.Properties._.deduplicate-!
d_deduplicate'45''33'_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
d_deduplicate'45''33'_78 ~v0 ~v1 v2 v3
  = du_deduplicate'45''33'_78 v2 v3
du_deduplicate'45''33'_78 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
du_deduplicate'45''33'_78 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C_'91''93'_22
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28
             (coe
                MAlonzo.Code.Data.List.Relation.Unary.All.Properties.du_all'45'filter_1400
                (coe
                   (\ v4 ->
                      coe
                        MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d__'8799'__52
                           (MAlonzo.Code.Relation.Binary.Bundles.d_isDecEquivalence_100
                              (coe v0))
                           v2 v4)))
                (coe
                   MAlonzo.Code.Data.List.Base.du_deduplicate_834
                   (MAlonzo.Code.Relation.Binary.Structures.d__'8799'__52
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isDecEquivalence_100
                         (coe v0)))
                   v3))
             (coe
                MAlonzo.Code.Data.List.Relation.Unary.Unique.Setoid.Properties.du_filter'8314'_462
                (\ v4 ->
                   coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d__'8799'__52
                        (MAlonzo.Code.Relation.Binary.Bundles.d_isDecEquivalence_100
                           (coe v0))
                        v2 v4))
                (coe
                   MAlonzo.Code.Data.List.Base.du_deduplicate_834
                   (MAlonzo.Code.Relation.Binary.Structures.d__'8799'__52
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isDecEquivalence_100
                         (coe v0)))
                   v3)
                (coe du_deduplicate'45''33'_78 (coe v0) (coe v3)))
      _ -> MAlonzo.RTE.mazUnreachableError
