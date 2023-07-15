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

module MAlonzo.Code.Class.Monad.Instances where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Class.Applicative.Core
import qualified MAlonzo.Code.Class.Applicative.Instances
import qualified MAlonzo.Code.Class.Functor.Core
import qualified MAlonzo.Code.Class.Functor.Instances
import qualified MAlonzo.Code.Class.Monad.Core
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Maybe.Base

-- Class.Monad.Instances.Monad-Maybe
d_Monad'45'Maybe_14 :: MAlonzo.Code.Class.Monad.Core.T_Monad_22
d_Monad'45'Maybe_14
  = coe
      MAlonzo.Code.Class.Monad.Core.C_Monad'46'constructor_377
      (coe
         MAlonzo.Code.Class.Applicative.Core.C_Applicative'46'constructor_377
         (coe MAlonzo.Code.Class.Functor.Instances.d_Functor'45'Maybe_12)
         (coe (\ v0 v1 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16))
         (coe
            (\ v0 v1 v2 v3 ->
               coe
                 MAlonzo.Code.Data.Maybe.Base.du_maybe_36
                 (coe
                    MAlonzo.Code.Class.Functor.Core.du_fmap_42
                    MAlonzo.Code.Class.Functor.Instances.d_Functor'45'Maybe_12 v0
                    erased v2 erased)
                 (let v4 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
                  coe (\ v5 -> v4)))))
      (coe (\ v0 v1 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16))
      (coe
         (\ v0 v1 v2 v3 v4 v5 ->
            coe
              MAlonzo.Code.Data.Maybe.Base.du_maybe_36 (coe v5)
              (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18) (coe v4)))
-- Class.Monad.Instances.MonadLaws-Maybe
d_MonadLaws'45'Maybe_22 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad'45'Laws_102
d_MonadLaws'45'Maybe_22 = erased
-- Class.Monad.Instances.Monad-List
d_Monad'45'List_30 :: MAlonzo.Code.Class.Monad.Core.T_Monad_22
d_Monad'45'List_30
  = coe
      MAlonzo.Code.Class.Monad.Core.C_Monad'46'constructor_377
      (coe
         MAlonzo.Code.Class.Applicative.Core.C_Applicative'46'constructor_377
         (coe MAlonzo.Code.Class.Functor.Instances.d_Functor'45'List_22)
         (coe (\ v0 v1 -> coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306))
         (coe
            (\ v0 v1 v2 v3 v4 v5 ->
               coe
                 MAlonzo.Code.Data.List.Base.du_concatMap_272
                 (coe
                    MAlonzo.Code.Class.Functor.Core.du__'60''38''62'__50
                    (coe MAlonzo.Code.Class.Functor.Instances.d_Functor'45'List_22)
                    (coe v0) (coe v2) (coe v5))
                 (coe v4))))
      (coe (\ v0 v1 -> coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306))
      (coe
         (\ v0 v1 v2 v3 v4 v5 ->
            coe
              MAlonzo.Code.Data.List.Base.du_concatMap_272 (coe v5) (coe v4)))
-- Class.Monad.Instances.Monad-TC
d_Monad'45'TC_34 :: MAlonzo.Code.Class.Monad.Core.T_Monad_22
d_Monad'45'TC_34
  = coe
      MAlonzo.Code.Class.Monad.Core.C_Monad'46'constructor_377
      (coe
         MAlonzo.Code.Class.Applicative.Instances.d_Applicative'45'TC_46)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316)
      (coe
         (\ v0 v1 v2 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 v0 v2 erased))
