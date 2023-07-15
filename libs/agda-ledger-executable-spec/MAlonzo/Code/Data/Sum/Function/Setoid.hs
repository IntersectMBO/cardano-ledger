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

module MAlonzo.Code.Data.Sum.Function.Setoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise
import qualified MAlonzo.Code.Function.Bijection
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Injection
import qualified MAlonzo.Code.Function.Inverse
import qualified MAlonzo.Code.Function.LeftInverse
import qualified MAlonzo.Code.Function.Surjection
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Data.Sum.Function.Setoid._._⊎-⟶_
d__'8846''45''10230'__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d__'8846''45''10230'__36 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                         ~v10 ~v11 v12 v13
  = du__'8846''45''10230'__36 v12 v13
du__'8846''45''10230'__36 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
du__'8846''45''10230'__36 v0 v1
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe du_fg_54 (coe v0) (coe v1))
      (coe du_fg'45'cong_56 (coe v0) (coe v1))
-- Data.Sum.Function.Setoid._._._._≈_
d__'8776'__48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> ()
d__'8776'__48 = erased
-- Data.Sum.Function.Setoid._._._._≈_
d__'8776'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> ()
d__'8776'__52 = erased
-- Data.Sum.Function.Setoid._._.fg
d_fg_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_fg_54 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_fg_54 v12 v13
du_fg_54 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_fg_54 v0 v1
  = coe
      MAlonzo.Code.Data.Sum.Base.du_map_84
      (coe
         MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v0))
      (coe
         MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v1))
-- Data.Sum.Function.Setoid._._.fg-cong
d_fg'45'cong_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34
d_fg'45'cong_56 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12 v13 v14 v15 v16
  = du_fg'45'cong_56 v12 v13 v14 v15 v16
du_fg'45'cong_56 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34
du_fg'45'cong_56 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8321'_64 v7
        -> case coe v2 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
               -> case coe v3 of
                    MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9
                      -> coe
                           MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8321'_64
                           (coe MAlonzo.Code.Function.Equality.d_cong_40 v0 v8 v9 v7)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8322'_70 v7
        -> case coe v2 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8
               -> case coe v3 of
                    MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9
                      -> coe
                           MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8322'_70
                           (coe MAlonzo.Code.Function.Equality.d_cong_40 v1 v8 v9 v7)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Sum.Function.Setoid._.inj₁ₛ
d_inj'8321''8347'_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_inj'8321''8347'_78 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_inj'8321''8347'_78
du_inj'8321''8347'_78 :: MAlonzo.Code.Function.Equality.T_Π_16
du_inj'8321''8347'_78
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
      (\ v0 v1 v2 ->
         coe
           MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8321'_64 v2)
-- Data.Sum.Function.Setoid._.inj₂ₛ
d_inj'8322''8347'_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_inj'8322''8347'_80 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_inj'8322''8347'_80
du_inj'8322''8347'_80 :: MAlonzo.Code.Function.Equality.T_Π_16
du_inj'8322''8347'_80
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)
      (\ v0 v1 v2 ->
         coe
           MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8322'_70 v2)
-- Data.Sum.Function.Setoid._.[_,_]ₛ
d_'91'_'44'_'93''8347'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_'91'_'44'_'93''8347'_88 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
                          v10
  = du_'91'_'44'_'93''8347'_88 v9 v10
du_'91'_'44'_'93''8347'_88 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
du_'91'_'44'_'93''8347'_88 v0 v1
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v0))
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v1)))
      (coe
         (\ v2 v3 v4 ->
            case coe v4 of
              MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8321'_64 v7
                -> case coe v2 of
                     MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
                       -> case coe v3 of
                            MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9
                              -> coe MAlonzo.Code.Function.Equality.d_cong_40 v0 v8 v9 v7
                            _ -> MAlonzo.RTE.mazUnreachableError
                     _ -> MAlonzo.RTE.mazUnreachableError
              MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8322'_70 v7
                -> case coe v2 of
                     MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8
                       -> case coe v3 of
                            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9
                              -> coe MAlonzo.Code.Function.Equality.d_cong_40 v1 v8 v9 v7
                            _ -> MAlonzo.RTE.mazUnreachableError
                     _ -> MAlonzo.RTE.mazUnreachableError
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Data.Sum.Function.Setoid._.swapₛ
d_swap'8347'_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_swap'8347'_120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_swap'8347'_120
du_swap'8347'_120 :: MAlonzo.Code.Function.Equality.T_Π_16
du_swap'8347'_120
  = coe
      du_'91'_'44'_'93''8347'_88 (coe du_inj'8322''8347'_80)
      (coe du_inj'8321''8347'_78)
-- Data.Sum.Function.Setoid._._⊎-equivalence_
d__'8846''45'equivalence__150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d__'8846''45'equivalence__150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                              ~v9 ~v10 ~v11 v12 v13
  = du__'8846''45'equivalence__150 v12 v13
du__'8846''45'equivalence__150 ::
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du__'8846''45'equivalence__150 v0 v1
  = coe
      MAlonzo.Code.Function.Equivalence.C_Equivalence'46'constructor_433
      (coe
         du__'8846''45''10230'__36
         (coe MAlonzo.Code.Function.Equivalence.d_to_34 (coe v0))
         (coe MAlonzo.Code.Function.Equivalence.d_to_34 (coe v1)))
      (coe
         du__'8846''45''10230'__36
         (coe MAlonzo.Code.Function.Equivalence.d_from_36 (coe v0))
         (coe MAlonzo.Code.Function.Equivalence.d_from_36 (coe v1)))
-- Data.Sum.Function.Setoid._._⊎-injection_
d__'8846''45'injection__160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88
d__'8846''45'injection__160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                            ~v10 ~v11 v12 v13
  = du__'8846''45'injection__160 v12 v13
du__'8846''45'injection__160 ::
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88
du__'8846''45'injection__160 v0 v1
  = coe
      MAlonzo.Code.Function.Injection.C_Injection'46'constructor_3039
      (coe
         du__'8846''45''10230'__36
         (coe MAlonzo.Code.Function.Injection.d_to_106 (coe v0))
         (coe MAlonzo.Code.Function.Injection.d_to_106 (coe v1)))
      (coe
         (\ v2 v3 -> coe du_inj_182 (coe v0) (coe v1) (coe v2) (coe v3)))
-- Data.Sum.Function.Setoid._._._._≈_
d__'8776'__172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> ()
d__'8776'__172 = erased
-- Data.Sum.Function.Setoid._._._._≈_
d__'8776'__176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> ()
d__'8776'__176 = erased
-- Data.Sum.Function.Setoid._._.inj
d_inj_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34
d_inj_182 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
          v14 v15 v16
  = du_inj_182 v12 v13 v14 v15 v16
du_inj_182 ::
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34 ->
  MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.T_Pointwise_34
du_inj_182 v0 v1 v2 v3 v4
  = case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
        -> case coe v3 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v6
               -> case coe v4 of
                    MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8321'_64 v9
                      -> coe
                           MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8321'_64
                           (coe MAlonzo.Code.Function.Injection.d_injective_108 v0 v5 v6 v9)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5
        -> case coe v3 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v6
               -> case coe v4 of
                    MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8322'_70 v9
                      -> coe
                           MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8322'_70
                           (coe MAlonzo.Code.Function.Injection.d_injective_108 v1 v5 v6 v9)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Sum.Function.Setoid._._⊎-left-inverse_
d__'8846''45'left'45'inverse__196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d__'8846''45'left'45'inverse__196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                  ~v8 ~v9 ~v10 ~v11 v12 v13
  = du__'8846''45'left'45'inverse__196 v12 v13
du__'8846''45'left'45'inverse__196 ::
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du__'8846''45'left'45'inverse__196 v0 v1
  = coe
      MAlonzo.Code.Function.LeftInverse.C_LeftInverse'46'constructor_4525
      (coe
         MAlonzo.Code.Function.Equivalence.d_to_34
         (coe du_eq_206 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Function.Equivalence.d_from_36
         (coe du_eq_206 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (coe
            (\ v2 ->
               coe
                 MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8321'_64
                 (coe
                    MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106 v0
                    v2)))
         (coe
            (\ v2 ->
               coe
                 MAlonzo.Code.Data.Sum.Relation.Binary.Pointwise.C_inj'8322'_70
                 (coe
                    MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106 v1
                    v2))))
-- Data.Sum.Function.Setoid._._.eq
d_eq_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_eq_206 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_eq_206 v12 v13
du_eq_206 ::
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_eq_206 v0 v1
  = coe
      du__'8846''45'equivalence__150
      (coe MAlonzo.Code.Function.LeftInverse.du_equivalence_186 (coe v0))
      (coe MAlonzo.Code.Function.LeftInverse.du_equivalence_186 (coe v1))
-- Data.Sum.Function.Setoid._._⊎-surjection_
d__'8846''45'surjection__240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
d__'8846''45'surjection__240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                             ~v9 ~v10 ~v11 v12 v13
  = du__'8846''45'surjection__240 v12 v13
du__'8846''45'surjection__240 ::
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
du__'8846''45'surjection__240 v0 v1
  = coe
      MAlonzo.Code.Function.Surjection.C_Surjection'46'constructor_2365
      (coe
         MAlonzo.Code.Function.LeftInverse.d_from_104
         (coe du_inv_250 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Function.Surjection.C_Surjective'46'constructor_1227
         (coe
            MAlonzo.Code.Function.LeftInverse.d_to_102
            (coe du_inv_250 (coe v0) (coe v1)))
         (coe
            MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106
            (coe du_inv_250 (coe v0) (coe v1))))
-- Data.Sum.Function.Setoid._._.inv
d_inv_250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_inv_250 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_inv_250 v12 v13
du_inv_250 ::
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_inv_250 v0 v1
  = coe
      du__'8846''45'left'45'inverse__196
      (coe
         MAlonzo.Code.Function.Surjection.du_right'45'inverse_82 (coe v0))
      (coe
         MAlonzo.Code.Function.Surjection.du_right'45'inverse_82 (coe v1))
-- Data.Sum.Function.Setoid._._⊎-inverse_
d__'8846''45'inverse__252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d__'8846''45'inverse__252 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9
                          v10 ~v11 v12 v13
  = du__'8846''45'inverse__252 v8 v10 v12 v13
du__'8846''45'inverse__252 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du__'8846''45'inverse__252 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Inverse.C_Inverse'46'constructor_3553
      (coe
         MAlonzo.Code.Function.Surjection.d_to_72
         (coe du_surj_262 (coe v0) (coe v1) (coe v2) (coe v3)))
      (coe
         MAlonzo.Code.Function.Surjection.d_from_38
         (coe
            MAlonzo.Code.Function.Surjection.d_surjective_74
            (coe du_surj_262 (coe v0) (coe v1) (coe v2) (coe v3))))
      (coe
         MAlonzo.Code.Function.Inverse.C__InverseOf_'46'constructor_2103
         (coe
            MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106
            (coe du_inv_264 (coe v2) (coe v3)))
         (coe
            MAlonzo.Code.Function.Surjection.d_right'45'inverse'45'of_40
            (coe
               MAlonzo.Code.Function.Surjection.d_surjective_74
               (coe du_surj_262 (coe v0) (coe v1) (coe v2) (coe v3)))))
-- Data.Sum.Function.Setoid._._.surj
d_surj_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
d_surj_262 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 v10 ~v11 v12 v13
  = du_surj_262 v8 v10 v12 v13
du_surj_262 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
du_surj_262 v0 v1 v2 v3
  = coe
      du__'8846''45'surjection__240
      (coe
         MAlonzo.Code.Function.Bijection.du_surjection_100
         (coe
            MAlonzo.Code.Function.Inverse.du_bijection_98 (coe v0) (coe v2)))
      (coe
         MAlonzo.Code.Function.Bijection.du_surjection_100
         (coe
            MAlonzo.Code.Function.Inverse.du_bijection_98 (coe v1) (coe v3)))
-- Data.Sum.Function.Setoid._._.inv
d_inv_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_inv_264 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_inv_264 v12 v13
du_inv_264 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_inv_264 v0 v1
  = coe
      du__'8846''45'left'45'inverse__196
      (coe MAlonzo.Code.Function.Inverse.du_left'45'inverse_90 (coe v0))
      (coe MAlonzo.Code.Function.Inverse.du_left'45'inverse_90 (coe v1))
