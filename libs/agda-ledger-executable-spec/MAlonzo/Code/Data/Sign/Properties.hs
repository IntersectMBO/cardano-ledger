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

module MAlonzo.Code.Data.Sign.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sign.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.Sign.Properties._.IsAbelianGroup
d_IsAbelianGroup_8 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsAlternativeMagma
d_IsAlternativeMagma_10 a0 = ()
-- Data.Sign.Properties._.IsBand
d_IsBand_12 a0 = ()
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring
d_IsCancellativeCommutativeSemiring_14 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsCommutativeMagma
d_IsCommutativeMagma_16 a0 = ()
-- Data.Sign.Properties._.IsCommutativeMonoid
d_IsCommutativeMonoid_18 a0 a1 = ()
-- Data.Sign.Properties._.IsCommutativeRing
d_IsCommutativeRing_20 a0 a1 a2 a3 a4 = ()
-- Data.Sign.Properties._.IsCommutativeSemigroup
d_IsCommutativeSemigroup_22 a0 = ()
-- Data.Sign.Properties._.IsCommutativeSemiring
d_IsCommutativeSemiring_24 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne
d_IsCommutativeSemiringWithoutOne_26 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsFlexibleMagma
d_IsFlexibleMagma_28 a0 = ()
-- Data.Sign.Properties._.IsGroup
d_IsGroup_30 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid
d_IsIdempotentCommutativeMonoid_32 a0 a1 = ()
-- Data.Sign.Properties._.IsIdempotentMagma
d_IsIdempotentMagma_34 a0 = ()
-- Data.Sign.Properties._.IsIdempotentSemiring
d_IsIdempotentSemiring_36 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsInvertibleMagma
d_IsInvertibleMagma_38 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsInvertibleUnitalMagma
d_IsInvertibleUnitalMagma_40 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsKleeneAlgebra
d_IsKleeneAlgebra_42 a0 a1 a2 a3 a4 = ()
-- Data.Sign.Properties._.IsLeftBolLoop
d_IsLeftBolLoop_44 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsLoop
d_IsLoop_46 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsMagma
d_IsMagma_48 a0 = ()
-- Data.Sign.Properties._.IsMedialMagma
d_IsMedialMagma_50 a0 = ()
-- Data.Sign.Properties._.IsMiddleBolLoop
d_IsMiddleBolLoop_52 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsMonoid
d_IsMonoid_54 a0 a1 = ()
-- Data.Sign.Properties._.IsMoufangLoop
d_IsMoufangLoop_56 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsNearSemiring
d_IsNearSemiring_58 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsNearring
d_IsNearring_60 a0 a1 a2 a3 a4 = ()
-- Data.Sign.Properties._.IsNonAssociativeRing
d_IsNonAssociativeRing_62 a0 a1 a2 a3 a4 = ()
-- Data.Sign.Properties._.IsQuasigroup
d_IsQuasigroup_64 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsQuasiring
d_IsQuasiring_66 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsRightBolLoop
d_IsRightBolLoop_68 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsRing
d_IsRing_70 a0 a1 a2 a3 a4 = ()
-- Data.Sign.Properties._.IsRingWithoutOne
d_IsRingWithoutOne_72 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsSelectiveMagma
d_IsSelectiveMagma_74 a0 = ()
-- Data.Sign.Properties._.IsSemigroup
d_IsSemigroup_76 a0 = ()
-- Data.Sign.Properties._.IsSemimedialMagma
d_IsSemimedialMagma_78 a0 = ()
-- Data.Sign.Properties._.IsSemiring
d_IsSemiring_80 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero
d_IsSemiringWithoutAnnihilatingZero_82 a0 a1 a2 a3 = ()
-- Data.Sign.Properties._.IsSemiringWithoutOne
d_IsSemiringWithoutOne_84 a0 a1 a2 = ()
-- Data.Sign.Properties._.IsUnitalMagma
d_IsUnitalMagma_86 a0 a1 = ()
-- Data.Sign.Properties._.IsAbelianGroup.assoc
d_assoc_92 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_92 = erased
-- Data.Sign.Properties._.IsAbelianGroup.comm
d_comm_94 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_94 = erased
-- Data.Sign.Properties._.IsAbelianGroup.identity
d_identity_96 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_96 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Sign.Properties._.IsAbelianGroup.inverse
d_inverse_102 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_102 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Sign.Properties._.IsAbelianGroup.isEquivalence
d_isEquivalence_114 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_114 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))))
-- Data.Sign.Properties._.IsAbelianGroup.isGroup
d_isGroup_116 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_116 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)
-- Data.Sign.Properties._.IsAbelianGroup.isMagma
d_isMagma_122 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_122 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))))
-- Data.Sign.Properties._.IsAbelianGroup.isMonoid
d_isMonoid_124 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_124 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Sign.Properties._.IsAbelianGroup.isSemigroup
d_isSemigroup_128 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_128 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Sign.Properties._.IsAbelianGroup.⁻¹-cong
d_'8315''185''45'cong_146 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_146 = erased
-- Data.Sign.Properties._.IsAbelianGroup.∙-cong
d_'8729''45'cong_148 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_148 = erased
-- Data.Sign.Properties._.IsAlternativeMagma.alter
d_alter_156 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alter_156 v0
  = coe MAlonzo.Code.Algebra.Structures.d_alter_258 (coe v0)
-- Data.Sign.Properties._.IsAlternativeMagma.isEquivalence
d_isEquivalence_162 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_162 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0))
-- Data.Sign.Properties._.IsAlternativeMagma.isMagma
d_isMagma_164 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_164 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0)
-- Data.Sign.Properties._.IsAlternativeMagma.∙-cong
d_'8729''45'cong_178 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_178 = erased
-- Data.Sign.Properties._.IsBand.assoc
d_assoc_186 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_186 = erased
-- Data.Sign.Properties._.IsBand.idem
d_idem_188 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_188 = erased
-- Data.Sign.Properties._.IsBand.isEquivalence
d_isEquivalence_190 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_190 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)))
-- Data.Sign.Properties._.IsBand.isMagma
d_isMagma_192 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_192 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0))
-- Data.Sign.Properties._.IsBand.isSemigroup
d_isSemigroup_196 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_196 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)
-- Data.Sign.Properties._.IsBand.∙-cong
d_'8729''45'cong_208 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_208 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.*-assoc
d_'42''45'assoc_216 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_216 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.*-cancelˡ-nonZero
d_'42''45'cancel'737''45'nonZero_218 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45'nonZero_218 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.*-comm
d_'42''45'comm_220 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_220 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.*-cong
d_'42''45'cong_222 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_222 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.*-identity
d_'42''45'identity_228 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.assoc
d_assoc_246 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_246 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.comm
d_comm_248 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_248 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.∙-cong
d_'8729''45'cong_250 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_250 = erased
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.identity
d_identity_256 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_256 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                     (coe v0))))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_264 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.isMagma
d_isMagma_268 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_268 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                        (coe v0)))))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.isMonoid
d_isMonoid_270 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_270 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                  (coe v0)))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.isSemigroup
d_isSemigroup_272 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_272 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                     (coe v0))))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.distrib
d_distrib_276 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_276 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.isCommutativeSemiring
d_isCommutativeSemiring_282 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_282 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
      (coe v0)
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.isEquivalence
d_isEquivalence_286 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_286 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                           (coe v0))))))))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.isSemiring
d_isSemiring_292 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_292 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe v0))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_294 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_294 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Sign.Properties._.IsCancellativeCommutativeSemiring.zero
d_zero_308 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_308 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Sign.Properties._.IsCommutativeMagma.comm
d_comm_316 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_316 = erased
-- Data.Sign.Properties._.IsCommutativeMagma.isEquivalence
d_isEquivalence_318 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_318 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0))
-- Data.Sign.Properties._.IsCommutativeMagma.isMagma
d_isMagma_320 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_320 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0)
-- Data.Sign.Properties._.IsCommutativeMagma.∙-cong
d_'8729''45'cong_334 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_334 = erased
-- Data.Sign.Properties._.IsCommutativeMonoid.assoc
d_assoc_342 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_342 = erased
-- Data.Sign.Properties._.IsCommutativeMonoid.comm
d_comm_344 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_344 = erased
-- Data.Sign.Properties._.IsCommutativeMonoid.identity
d_identity_346 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_346 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Sign.Properties._.IsCommutativeMonoid.isEquivalence
d_isEquivalence_356 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_356 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))))
-- Data.Sign.Properties._.IsCommutativeMonoid.isMagma
d_isMagma_358 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_358 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)))
-- Data.Sign.Properties._.IsCommutativeMonoid.isMonoid
d_isMonoid_360 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_360 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)
-- Data.Sign.Properties._.IsCommutativeMonoid.isSemigroup
d_isSemigroup_364 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_364 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Sign.Properties._.IsCommutativeMonoid.∙-cong
d_'8729''45'cong_378 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_378 = erased
-- Data.Sign.Properties._.IsCommutativeRing.*-assoc
d_'42''45'assoc_388 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_388 = erased
-- Data.Sign.Properties._.IsCommutativeRing.*-comm
d_'42''45'comm_390 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_390 = erased
-- Data.Sign.Properties._.IsCommutativeRing.*-cong
d_'42''45'cong_392 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_392 = erased
-- Data.Sign.Properties._.IsCommutativeRing.*-identity
d_'42''45'identity_398 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_398 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Sign.Properties._.IsCommutativeRing.assoc
d_assoc_416 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_416 = erased
-- Data.Sign.Properties._.IsCommutativeRing.comm
d_comm_418 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_418 = erased
-- Data.Sign.Properties._.IsCommutativeRing.∙-cong
d_'8729''45'cong_420 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_420 = erased
-- Data.Sign.Properties._.IsCommutativeRing.identity
d_identity_426 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_426 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Sign.Properties._.IsCommutativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_432 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_432 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Sign.Properties._.IsCommutativeRing.isGroup
d_isGroup_440 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_440 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))
-- Data.Sign.Properties._.IsCommutativeRing.isMagma
d_isMagma_446 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_446 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))))
-- Data.Sign.Properties._.IsCommutativeRing.isMonoid
d_isMonoid_448 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_448 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Sign.Properties._.IsCommutativeRing.isSemigroup
d_isSemigroup_450 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_450 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Sign.Properties._.IsCommutativeRing.⁻¹-cong
d_'8315''185''45'cong_454 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_454 = erased
-- Data.Sign.Properties._.IsCommutativeRing.inverse
d_inverse_456 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_456 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Sign.Properties._.IsCommutativeRing.distrib
d_distrib_462 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_462 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2426
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Sign.Properties._.IsCommutativeRing.isEquivalence
d_isEquivalence_472 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_472 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))))
-- Data.Sign.Properties._.IsCommutativeRing.isRing
d_isRing_478 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_478 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)
-- Data.Sign.Properties._.IsCommutativeRing.zero
d_zero_500 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_500 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2428
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Sign.Properties._.IsCommutativeSemigroup.assoc
d_assoc_508 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_508 = erased
-- Data.Sign.Properties._.IsCommutativeSemigroup.comm
d_comm_510 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_510 = erased
-- Data.Sign.Properties._.IsCommutativeSemigroup.isEquivalence
d_isEquivalence_514 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_514 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)))
-- Data.Sign.Properties._.IsCommutativeSemigroup.isMagma
d_isMagma_516 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_516 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0))
-- Data.Sign.Properties._.IsCommutativeSemigroup.isSemigroup
d_isSemigroup_520 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_520 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)
-- Data.Sign.Properties._.IsCommutativeSemigroup.∙-cong
d_'8729''45'cong_532 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_532 = erased
-- Data.Sign.Properties._.IsCommutativeSemiring.*-assoc
d_'42''45'assoc_540 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_540 = erased
-- Data.Sign.Properties._.IsCommutativeSemiring.*-comm
d_'42''45'comm_542 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_542 = erased
-- Data.Sign.Properties._.IsCommutativeSemiring.*-cong
d_'42''45'cong_544 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_544 = erased
-- Data.Sign.Properties._.IsCommutativeSemiring.*-identity
d_'42''45'identity_550 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_550 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Sign.Properties._.IsCommutativeSemiring.assoc
d_assoc_568 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_568 = erased
-- Data.Sign.Properties._.IsCommutativeSemiring.comm
d_comm_570 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_570 = erased
-- Data.Sign.Properties._.IsCommutativeSemiring.∙-cong
d_'8729''45'cong_572 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_572 = erased
-- Data.Sign.Properties._.IsCommutativeSemiring.identity
d_identity_578 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_578 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Sign.Properties._.IsCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_586 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_586 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Sign.Properties._.IsCommutativeSemiring.isMagma
d_isMagma_590 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_590 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))))))
-- Data.Sign.Properties._.IsCommutativeSemiring.isMonoid
d_isMonoid_592 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_592 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))))
-- Data.Sign.Properties._.IsCommutativeSemiring.isSemigroup
d_isSemigroup_594 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_594 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Sign.Properties._.IsCommutativeSemiring.distrib
d_distrib_598 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_598 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Sign.Properties._.IsCommutativeSemiring.isEquivalence
d_isEquivalence_606 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_606 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))))
-- Data.Sign.Properties._.IsCommutativeSemiring.isSemiring
d_isSemiring_612 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_612 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)
-- Data.Sign.Properties._.IsCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_614 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_614 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Sign.Properties._.IsCommutativeSemiring.zero
d_zero_628 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_628 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.*-assoc
d_'42''45'assoc_636 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_636 = erased
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.*-comm
d_'42''45'comm_638 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_638 = erased
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.*-cong
d_'42''45'cong_640 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_640 = erased
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.comm
d_comm_654 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_654 = erased
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_658 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_658 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.isMonoid
d_isMonoid_662 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_662 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
            (coe v0)))
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.distrib
d_distrib_664 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_664 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1166
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.isEquivalence
d_isEquivalence_666 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_666 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
                     (coe v0))))))
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.isSemiringWithoutOne
d_isSemiringWithoutOne_670 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_670 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
      (coe v0)
-- Data.Sign.Properties._.IsCommutativeSemiringWithoutOne.zero
d_zero_672 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_672 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1168
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Sign.Properties._.IsFlexibleMagma.flex
d_flex_680 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_flex_680 = erased
-- Data.Sign.Properties._.IsFlexibleMagma.isEquivalence
d_isEquivalence_682 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_682 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0))
-- Data.Sign.Properties._.IsFlexibleMagma.isMagma
d_isMagma_684 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_684 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0)
-- Data.Sign.Properties._.IsFlexibleMagma.∙-cong
d_'8729''45'cong_698 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_698 = erased
-- Data.Sign.Properties._.IsGroup.assoc
d_assoc_708 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_708 = erased
-- Data.Sign.Properties._.IsGroup.identity
d_identity_710 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_710 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Sign.Properties._.IsGroup.inverse
d_inverse_716 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_716 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_904 (coe v0)
-- Data.Sign.Properties._.IsGroup.isEquivalence
d_isEquivalence_722 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_722 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))))
-- Data.Sign.Properties._.IsGroup.isMagma
d_isMagma_728 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_728 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)))
-- Data.Sign.Properties._.IsGroup.isMonoid
d_isMonoid_730 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_730 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)
-- Data.Sign.Properties._.IsGroup.isSemigroup
d_isSemigroup_734 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_734 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Sign.Properties._.IsGroup.⁻¹-cong
d_'8315''185''45'cong_752 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_752 = erased
-- Data.Sign.Properties._.IsGroup.∙-cong
d_'8729''45'cong_754 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_754 = erased
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.assoc
d_assoc_762 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_762 = erased
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.comm
d_comm_764 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_764 = erased
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.idem
d_idem_766 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_766 = erased
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.identity
d_identity_768 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_768 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.isCommutativeMonoid
d_isCommutativeMonoid_778 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_778 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0)
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.isEquivalence
d_isEquivalence_782 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_782 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe v0)))))
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.isMagma
d_isMagma_784 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_784 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.isMonoid
d_isMonoid_786 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_786 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.isSemigroup
d_isSemigroup_790 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_790 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Sign.Properties._.IsIdempotentCommutativeMonoid.∙-cong
d_'8729''45'cong_804 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_804 = erased
-- Data.Sign.Properties._.IsIdempotentMagma.idem
d_idem_812 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_812 = erased
-- Data.Sign.Properties._.IsIdempotentMagma.isEquivalence
d_isEquivalence_814 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_814 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0))
-- Data.Sign.Properties._.IsIdempotentMagma.isMagma
d_isMagma_816 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_816 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0)
-- Data.Sign.Properties._.IsIdempotentMagma.∙-cong
d_'8729''45'cong_830 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_830 = erased
-- Data.Sign.Properties._.IsIdempotentSemiring.*-assoc
d_'42''45'assoc_838 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_838 = erased
-- Data.Sign.Properties._.IsIdempotentSemiring.*-cong
d_'42''45'cong_840 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_840 = erased
-- Data.Sign.Properties._.IsIdempotentSemiring.*-identity
d_'42''45'identity_846 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_846 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Sign.Properties._.IsIdempotentSemiring.assoc
d_assoc_858 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_858 = erased
-- Data.Sign.Properties._.IsIdempotentSemiring.comm
d_comm_860 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_860 = erased
-- Data.Sign.Properties._.IsIdempotentSemiring.∙-cong
d_'8729''45'cong_862 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_862 = erased
-- Data.Sign.Properties._.IsIdempotentSemiring.+-idem
d_'43''45'idem_868 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_868 = erased
-- Data.Sign.Properties._.IsIdempotentSemiring.identity
d_identity_870 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_870 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Sign.Properties._.IsIdempotentSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_878 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_878 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Sign.Properties._.IsIdempotentSemiring.isMagma
d_isMagma_882 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_882 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))))))
-- Data.Sign.Properties._.IsIdempotentSemiring.isMonoid
d_isMonoid_884 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_884 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))))
-- Data.Sign.Properties._.IsIdempotentSemiring.isSemigroup
d_isSemigroup_886 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_886 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Sign.Properties._.IsIdempotentSemiring.distrib
d_distrib_890 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_890 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Sign.Properties._.IsIdempotentSemiring.isEquivalence
d_isEquivalence_896 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_896 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))))
-- Data.Sign.Properties._.IsIdempotentSemiring.isSemiring
d_isSemiring_902 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_902 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)
-- Data.Sign.Properties._.IsIdempotentSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_904 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_904 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Sign.Properties._.IsIdempotentSemiring.zero
d_zero_918 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_918 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Sign.Properties._.IsInvertibleMagma.inverse
d_inverse_926 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_926 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_792 (coe v0)
-- Data.Sign.Properties._.IsInvertibleMagma.isEquivalence
d_isEquivalence_932 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_932 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0))
-- Data.Sign.Properties._.IsInvertibleMagma.isMagma
d_isMagma_934 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_934 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0)
-- Data.Sign.Properties._.IsInvertibleMagma.⁻¹-cong
d_'8315''185''45'cong_948 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_948 = erased
-- Data.Sign.Properties._.IsInvertibleMagma.∙-cong
d_'8729''45'cong_950 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_950 = erased
-- Data.Sign.Properties._.IsInvertibleUnitalMagma.identity
d_identity_958 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_958 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_842 (coe v0)
-- Data.Sign.Properties._.IsInvertibleUnitalMagma.inverse
d_inverse_964 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_964 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_792
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Sign.Properties._.IsInvertibleUnitalMagma.isEquivalence
d_isEquivalence_970 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_970 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe
            MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)))
-- Data.Sign.Properties._.IsInvertibleUnitalMagma.isInvertibleMagma
d_isInvertibleMagma_972 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_972 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)
-- Data.Sign.Properties._.IsInvertibleUnitalMagma.isMagma
d_isMagma_974 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_974 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_790
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Sign.Properties._.IsInvertibleUnitalMagma.⁻¹-cong
d_'8315''185''45'cong_990 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_990 = erased
-- Data.Sign.Properties._.IsInvertibleUnitalMagma.∙-cong
d_'8729''45'cong_992 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_992 = erased
-- Data.Sign.Properties._.IsKleeneAlgebra.*-assoc
d_'42''45'assoc_1000 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1000 = erased
-- Data.Sign.Properties._.IsKleeneAlgebra.*-cong
d_'42''45'cong_1002 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1002 = erased
-- Data.Sign.Properties._.IsKleeneAlgebra.*-identity
d_'42''45'identity_1008 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1008 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Sign.Properties._.IsKleeneAlgebra.assoc
d_assoc_1020 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1020 = erased
-- Data.Sign.Properties._.IsKleeneAlgebra.comm
d_comm_1022 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1022 = erased
-- Data.Sign.Properties._.IsKleeneAlgebra.∙-cong
d_'8729''45'cong_1024 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1024 = erased
-- Data.Sign.Properties._.IsKleeneAlgebra.+-idem
d_'43''45'idem_1030 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_1030 = erased
-- Data.Sign.Properties._.IsKleeneAlgebra.identity
d_identity_1032 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1032 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                     (coe v0))))))
-- Data.Sign.Properties._.IsKleeneAlgebra.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1040 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1040 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Sign.Properties._.IsKleeneAlgebra.isMagma
d_isMagma_1044 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                        (coe v0)))))))
-- Data.Sign.Properties._.IsKleeneAlgebra.isMonoid
d_isMonoid_1046 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1046 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                  (coe v0)))))
-- Data.Sign.Properties._.IsKleeneAlgebra.isSemigroup
d_isSemigroup_1048 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1048 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                     (coe v0))))))
-- Data.Sign.Properties._.IsKleeneAlgebra.distrib
d_distrib_1052 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1052 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Sign.Properties._.IsKleeneAlgebra.isEquivalence
d_isEquivalence_1058 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                           (coe v0))))))))
-- Data.Sign.Properties._.IsKleeneAlgebra.isIdempotentSemiring
d_isIdempotentSemiring_1060 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_1060 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
      (coe v0)
-- Data.Sign.Properties._.IsKleeneAlgebra.isSemiring
d_isSemiring_1066 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1066 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
      (coe
         MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe v0))
-- Data.Sign.Properties._.IsKleeneAlgebra.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1068 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1068 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Sign.Properties._.IsKleeneAlgebra.starDestructive
d_starDestructive_1078 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starDestructive_1078 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_starDestructive_1856 (coe v0)
-- Data.Sign.Properties._.IsKleeneAlgebra.starExpansive
d_starExpansive_1084 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starExpansive_1084 v0
  = coe MAlonzo.Code.Algebra.Structures.d_starExpansive_1854 (coe v0)
-- Data.Sign.Properties._.IsKleeneAlgebra.zero
d_zero_1094 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1094 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Sign.Properties._.IsLeftBolLoop.//-cong
d_'47''47''45'cong_1102 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1102 = erased
-- Data.Sign.Properties._.IsLeftBolLoop.\\-cong
d_'92''92''45'cong_1108 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1108 = erased
-- Data.Sign.Properties._.IsLeftBolLoop.identity
d_identity_1114 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1114 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Sign.Properties._.IsLeftBolLoop.isEquivalence
d_isEquivalence_1120 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1120 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))))
-- Data.Sign.Properties._.IsLeftBolLoop.isLoop
d_isLoop_1122 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1122 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)
-- Data.Sign.Properties._.IsLeftBolLoop.isMagma
d_isMagma_1124 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1124 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Sign.Properties._.IsLeftBolLoop.isQuasigroup
d_isQuasigroup_1128 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1128 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Sign.Properties._.IsLeftBolLoop.leftBol
d_leftBol_1130 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1130 = erased
-- Data.Sign.Properties._.IsLeftBolLoop.leftDivides
d_leftDivides_1132 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Sign.Properties._.IsLeftBolLoop.rightDivides
d_rightDivides_1142 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1142 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Sign.Properties._.IsLeftBolLoop.∙-cong
d_'8729''45'cong_1154 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1154 = erased
-- Data.Sign.Properties._.IsLoop.//-cong
d_'47''47''45'cong_1162 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1162 = erased
-- Data.Sign.Properties._.IsLoop.\\-cong
d_'92''92''45'cong_1168 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1168 = erased
-- Data.Sign.Properties._.IsLoop.identity
d_identity_1174 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1174 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2784 (coe v0)
-- Data.Sign.Properties._.IsLoop.isEquivalence
d_isEquivalence_1180 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1180 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)))
-- Data.Sign.Properties._.IsLoop.isMagma
d_isMagma_1182 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1182 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Sign.Properties._.IsLoop.isQuasigroup
d_isQuasigroup_1186 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1186 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)
-- Data.Sign.Properties._.IsLoop.leftDivides
d_leftDivides_1188 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1188 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Sign.Properties._.IsLoop.rightDivides
d_rightDivides_1198 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1198 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Sign.Properties._.IsLoop.∙-cong
d_'8729''45'cong_1210 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1210 = erased
-- Data.Sign.Properties._.IsMagma.isEquivalence
d_isEquivalence_1218 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1218 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v0)
-- Data.Sign.Properties._.IsMagma.∙-cong
d_'8729''45'cong_1232 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1232 = erased
-- Data.Sign.Properties._.IsMedialMagma.isEquivalence
d_isEquivalence_1240 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0))
-- Data.Sign.Properties._.IsMedialMagma.isMagma
d_isMagma_1242 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1242 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0)
-- Data.Sign.Properties._.IsMedialMagma.medial
d_medial_1246 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_medial_1246 = erased
-- Data.Sign.Properties._.IsMedialMagma.∙-cong
d_'8729''45'cong_1258 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1258 = erased
-- Data.Sign.Properties._.IsMiddleBolLoop.//-cong
d_'47''47''45'cong_1266 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1266 = erased
-- Data.Sign.Properties._.IsMiddleBolLoop.\\-cong
d_'92''92''45'cong_1272 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1272 = erased
-- Data.Sign.Properties._.IsMiddleBolLoop.identity
d_identity_1278 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1278 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Sign.Properties._.IsMiddleBolLoop.isEquivalence
d_isEquivalence_1284 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1284 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))))
-- Data.Sign.Properties._.IsMiddleBolLoop.isLoop
d_isLoop_1286 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1286 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)
-- Data.Sign.Properties._.IsMiddleBolLoop.isMagma
d_isMagma_1288 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1288 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Sign.Properties._.IsMiddleBolLoop.isQuasigroup
d_isQuasigroup_1292 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1292 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Sign.Properties._.IsMiddleBolLoop.leftDivides
d_leftDivides_1294 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1294 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Sign.Properties._.IsMiddleBolLoop.middleBol
d_middleBol_1300 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_middleBol_1300 = erased
-- Data.Sign.Properties._.IsMiddleBolLoop.rightDivides
d_rightDivides_1306 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Sign.Properties._.IsMiddleBolLoop.∙-cong
d_'8729''45'cong_1318 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1318 = erased
-- Data.Sign.Properties._.IsMonoid.assoc
d_assoc_1326 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1326 = erased
-- Data.Sign.Properties._.IsMonoid.identity
d_identity_1328 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1328 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_612 (coe v0)
-- Data.Sign.Properties._.IsMonoid.isEquivalence
d_isEquivalence_1334 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1334 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)))
-- Data.Sign.Properties._.IsMonoid.isMagma
d_isMagma_1336 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0))
-- Data.Sign.Properties._.IsMonoid.isSemigroup
d_isSemigroup_1340 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1340 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)
-- Data.Sign.Properties._.IsMonoid.∙-cong
d_'8729''45'cong_1354 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1354 = erased
-- Data.Sign.Properties._.IsMoufangLoop.//-cong
d_'47''47''45'cong_1362 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1362 = erased
-- Data.Sign.Properties._.IsMoufangLoop.\\-cong
d_'92''92''45'cong_1368 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1368 = erased
-- Data.Sign.Properties._.IsMoufangLoop.identical
d_identical_1374 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identical_1374 = erased
-- Data.Sign.Properties._.IsMoufangLoop.identity
d_identity_1376 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1376 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Sign.Properties._.IsMoufangLoop.isEquivalence
d_isEquivalence_1382 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1382 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2860
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))))
-- Data.Sign.Properties._.IsMoufangLoop.isLeftBolLoop
d_isLeftBolLoop_1384 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
d_isLeftBolLoop_1384 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)
-- Data.Sign.Properties._.IsMoufangLoop.isLoop
d_isLoop_1386 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1386 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2860
      (coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))
-- Data.Sign.Properties._.IsMoufangLoop.isMagma
d_isMagma_1388 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1388 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Sign.Properties._.IsMoufangLoop.isQuasigroup
d_isQuasigroup_1392 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1392 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Sign.Properties._.IsMoufangLoop.leftBol
d_leftBol_1394 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1394 = erased
-- Data.Sign.Properties._.IsMoufangLoop.leftDivides
d_leftDivides_1396 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1396 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Sign.Properties._.IsMoufangLoop.rightBol
d_rightBol_1406 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1406 = erased
-- Data.Sign.Properties._.IsMoufangLoop.rightDivides
d_rightDivides_1408 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1408 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Sign.Properties._.IsMoufangLoop.∙-cong
d_'8729''45'cong_1420 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1420 = erased
-- Data.Sign.Properties._.IsNearSemiring.*-assoc
d_'42''45'assoc_1428 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1428 = erased
-- Data.Sign.Properties._.IsNearSemiring.*-cong
d_'42''45'cong_1430 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1430 = erased
-- Data.Sign.Properties._.IsNearSemiring.assoc
d_assoc_1440 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1440 = erased
-- Data.Sign.Properties._.IsNearSemiring.∙-cong
d_'8729''45'cong_1442 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1442 = erased
-- Data.Sign.Properties._.IsNearSemiring.identity
d_identity_1448 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1448 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Sign.Properties._.IsNearSemiring.isMagma
d_isMagma_1454 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1454 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)))
-- Data.Sign.Properties._.IsNearSemiring.+-isMonoid
d_'43''45'isMonoid_1456 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1456 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)
-- Data.Sign.Properties._.IsNearSemiring.isSemigroup
d_isSemigroup_1458 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1458 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Sign.Properties._.IsNearSemiring.distribʳ
d_distrib'691'_1462 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691'_1462 = erased
-- Data.Sign.Properties._.IsNearSemiring.isEquivalence
d_isEquivalence_1464 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1464 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))))
-- Data.Sign.Properties._.IsNearSemiring.zeroˡ
d_zero'737'_1478 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zero'737'_1478 = erased
-- Data.Sign.Properties._.IsNearring.*-assoc
d_'42''45'assoc_1482 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1482 = erased
-- Data.Sign.Properties._.IsNearring.*-cong
d_'42''45'cong_1484 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1484 = erased
-- Data.Sign.Properties._.IsNearring.*-identity
d_'42''45'identity_1490 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1490 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Sign.Properties._.IsNearring.assoc
d_assoc_1502 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1502 = erased
-- Data.Sign.Properties._.IsNearring.∙-cong
d_'8729''45'cong_1504 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1504 = erased
-- Data.Sign.Properties._.IsNearring.identity
d_identity_1510 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1510 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Sign.Properties._.IsNearring.+-inverse
d_'43''45'inverse_1516 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_1516 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'inverse_2314 (coe v0)
-- Data.Sign.Properties._.IsNearring.isMagma
d_isMagma_1522 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1522 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))))
-- Data.Sign.Properties._.IsNearring.+-isMonoid
d_'43''45'isMonoid_1524 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1524 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Sign.Properties._.IsNearring.isSemigroup
d_isSemigroup_1526 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1526 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Sign.Properties._.IsNearring.distrib
d_distrib_1530 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1530 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1992
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Sign.Properties._.IsNearring.isEquivalence
d_isEquivalence_1532 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1532 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))))
-- Data.Sign.Properties._.IsNearring.isQuasiring
d_isQuasiring_1536 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
d_isQuasiring_1536 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)
-- Data.Sign.Properties._.IsNearring.zero
d_zero_1548 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1548 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1994
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Sign.Properties._.IsNearring.⁻¹-cong
d_'8315''185''45'cong_1550 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1550 = erased
-- Data.Sign.Properties._.IsNonAssociativeRing.*-cong
d_'42''45'cong_1556 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1556 = erased
-- Data.Sign.Properties._.IsNonAssociativeRing.assoc
d_assoc_1564 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1564 = erased
-- Data.Sign.Properties._.IsNonAssociativeRing.comm
d_comm_1566 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1566 = erased
-- Data.Sign.Properties._.IsNonAssociativeRing.∙-cong
d_'8729''45'cong_1568 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1568 = erased
-- Data.Sign.Properties._.IsNonAssociativeRing.identity
d_identity_1574 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Sign.Properties._.IsNonAssociativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1580 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1580 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
      (coe v0)
-- Data.Sign.Properties._.IsNonAssociativeRing.isGroup
d_isGroup_1588 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1588 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe v0))
-- Data.Sign.Properties._.IsNonAssociativeRing.isMagma
d_isMagma_1594 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1594 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                  (coe v0)))))
-- Data.Sign.Properties._.IsNonAssociativeRing.isMonoid
d_isMonoid_1596 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1596 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Sign.Properties._.IsNonAssociativeRing.isSemigroup
d_isSemigroup_1598 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1598 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Sign.Properties._.IsNonAssociativeRing.⁻¹-cong
d_'8315''185''45'cong_1602 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1602 = erased
-- Data.Sign.Properties._.IsNonAssociativeRing.inverse
d_inverse_1604 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1604 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Sign.Properties._.IsNonAssociativeRing.distrib
d_distrib_1610 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1610 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2208 (coe v0)
-- Data.Sign.Properties._.IsNonAssociativeRing.identity
d_identity_1612 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1612 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2206 (coe v0)
-- Data.Sign.Properties._.IsNonAssociativeRing.isEquivalence
d_isEquivalence_1614 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1614 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                     (coe v0))))))
-- Data.Sign.Properties._.IsNonAssociativeRing.zero
d_zero_1632 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1632 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2210 (coe v0)
-- Data.Sign.Properties._.IsQuasigroup.//-cong
d_'47''47''45'cong_1636 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1636 = erased
-- Data.Sign.Properties._.IsQuasigroup.\\-cong
d_'92''92''45'cong_1642 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1642 = erased
-- Data.Sign.Properties._.IsQuasigroup.isEquivalence
d_isEquivalence_1648 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1648 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0))
-- Data.Sign.Properties._.IsQuasigroup.isMagma
d_isMagma_1650 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1650 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0)
-- Data.Sign.Properties._.IsQuasigroup.leftDivides
d_leftDivides_1654 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1654 v0
  = coe MAlonzo.Code.Algebra.Structures.d_leftDivides_2710 (coe v0)
-- Data.Sign.Properties._.IsQuasigroup.rightDivides
d_rightDivides_1664 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1664 v0
  = coe MAlonzo.Code.Algebra.Structures.d_rightDivides_2712 (coe v0)
-- Data.Sign.Properties._.IsQuasigroup.∙-cong
d_'8729''45'cong_1676 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1676 = erased
-- Data.Sign.Properties._.IsQuasiring.*-assoc
d_'42''45'assoc_1684 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1684 = erased
-- Data.Sign.Properties._.IsQuasiring.*-cong
d_'42''45'cong_1686 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1686 = erased
-- Data.Sign.Properties._.IsQuasiring.*-identity
d_'42''45'identity_1692 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1692 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990 (coe v0)
-- Data.Sign.Properties._.IsQuasiring.assoc
d_assoc_1704 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1704 = erased
-- Data.Sign.Properties._.IsQuasiring.∙-cong
d_'8729''45'cong_1706 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1706 = erased
-- Data.Sign.Properties._.IsQuasiring.identity
d_identity_1712 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1712 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Sign.Properties._.IsQuasiring.isMagma
d_isMagma_1718 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)))
-- Data.Sign.Properties._.IsQuasiring.+-isMonoid
d_'43''45'isMonoid_1720 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1720 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)
-- Data.Sign.Properties._.IsQuasiring.isSemigroup
d_isSemigroup_1722 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1722 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Sign.Properties._.IsQuasiring.distrib
d_distrib_1726 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1726 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1992 (coe v0)
-- Data.Sign.Properties._.IsQuasiring.isEquivalence
d_isEquivalence_1728 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1728 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))))
-- Data.Sign.Properties._.IsQuasiring.zero
d_zero_1742 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1742 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1994 (coe v0)
-- Data.Sign.Properties._.IsRightBolLoop.//-cong
d_'47''47''45'cong_1746 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1746 = erased
-- Data.Sign.Properties._.IsRightBolLoop.\\-cong
d_'92''92''45'cong_1752 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1752 = erased
-- Data.Sign.Properties._.IsRightBolLoop.identity
d_identity_1758 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1758 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Sign.Properties._.IsRightBolLoop.isEquivalence
d_isEquivalence_1764 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1764 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))))
-- Data.Sign.Properties._.IsRightBolLoop.isLoop
d_isLoop_1766 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1766 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)
-- Data.Sign.Properties._.IsRightBolLoop.isMagma
d_isMagma_1768 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1768 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Sign.Properties._.IsRightBolLoop.isQuasigroup
d_isQuasigroup_1772 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1772 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Sign.Properties._.IsRightBolLoop.leftDivides
d_leftDivides_1774 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1774 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Sign.Properties._.IsRightBolLoop.rightBol
d_rightBol_1784 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1784 = erased
-- Data.Sign.Properties._.IsRightBolLoop.rightDivides
d_rightDivides_1786 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1786 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Sign.Properties._.IsRightBolLoop.∙-cong
d_'8729''45'cong_1798 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1798 = erased
-- Data.Sign.Properties._.IsRing.*-assoc
d_'42''45'assoc_1808 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1808 = erased
-- Data.Sign.Properties._.IsRing.*-cong
d_'42''45'cong_1810 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1810 = erased
-- Data.Sign.Properties._.IsRing.*-identity
d_'42''45'identity_1816 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1816 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424 (coe v0)
-- Data.Sign.Properties._.IsRing.assoc
d_assoc_1828 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1828 = erased
-- Data.Sign.Properties._.IsRing.comm
d_comm_1830 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1830 = erased
-- Data.Sign.Properties._.IsRing.∙-cong
d_'8729''45'cong_1832 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1832 = erased
-- Data.Sign.Properties._.IsRing.identity
d_identity_1838 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1838 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Sign.Properties._.IsRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1844 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1844 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe v0)
-- Data.Sign.Properties._.IsRing.isGroup
d_isGroup_1852 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1852 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe v0))
-- Data.Sign.Properties._.IsRing.isMagma
d_isMagma_1858 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1858 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe v0)))))
-- Data.Sign.Properties._.IsRing.isMonoid
d_isMonoid_1860 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1860 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Sign.Properties._.IsRing.isSemigroup
d_isSemigroup_1862 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1862 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Sign.Properties._.IsRing.⁻¹-cong
d_'8315''185''45'cong_1866 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1866 = erased
-- Data.Sign.Properties._.IsRing.inverse
d_inverse_1868 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1868 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Sign.Properties._.IsRing.distrib
d_distrib_1874 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1874 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2426 (coe v0)
-- Data.Sign.Properties._.IsRing.isEquivalence
d_isEquivalence_1880 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1880 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe v0))))))
-- Data.Sign.Properties._.IsRing.zero
d_zero_1906 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1906 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2428 (coe v0)
-- Data.Sign.Properties._.IsRingWithoutOne.*-assoc
d_'42''45'assoc_1916 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1916 = erased
-- Data.Sign.Properties._.IsRingWithoutOne.*-cong
d_'42''45'cong_1918 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1918 = erased
-- Data.Sign.Properties._.IsRingWithoutOne.assoc
d_assoc_1928 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1928 = erased
-- Data.Sign.Properties._.IsRingWithoutOne.comm
d_comm_1930 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1930 = erased
-- Data.Sign.Properties._.IsRingWithoutOne.∙-cong
d_'8729''45'cong_1932 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1932 = erased
-- Data.Sign.Properties._.IsRingWithoutOne.identity
d_identity_1938 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1938 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Sign.Properties._.IsRingWithoutOne.+-isAbelianGroup
d_'43''45'isAbelianGroup_1944 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1944 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
      (coe v0)
-- Data.Sign.Properties._.IsRingWithoutOne.isGroup
d_isGroup_1952 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1952 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe v0))
-- Data.Sign.Properties._.IsRingWithoutOne.isMagma
d_isMagma_1958 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1958 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                  (coe v0)))))
-- Data.Sign.Properties._.IsRingWithoutOne.isMonoid
d_isMonoid_1960 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1960 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Sign.Properties._.IsRingWithoutOne.isSemigroup
d_isSemigroup_1962 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1962 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Sign.Properties._.IsRingWithoutOne.⁻¹-cong
d_'8315''185''45'cong_1966 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1966 = erased
-- Data.Sign.Properties._.IsRingWithoutOne.inverse
d_inverse_1968 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1968 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Sign.Properties._.IsRingWithoutOne.distrib
d_distrib_1974 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1974 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2082 (coe v0)
-- Data.Sign.Properties._.IsRingWithoutOne.isEquivalence
d_isEquivalence_1980 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1980 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                     (coe v0))))))
-- Data.Sign.Properties._.IsRingWithoutOne.zero
d_zero_1998 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1998 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2084 (coe v0)
-- Data.Sign.Properties._.IsSelectiveMagma.isEquivalence
d_isEquivalence_2006 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2006 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0))
-- Data.Sign.Properties._.IsSelectiveMagma.isMagma
d_isMagma_2008 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2008 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0)
-- Data.Sign.Properties._.IsSelectiveMagma.sel
d_sel_2016 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_2016 v0
  = coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0)
-- Data.Sign.Properties._.IsSelectiveMagma.∙-cong
d_'8729''45'cong_2024 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2024 = erased
-- Data.Sign.Properties._.IsSemigroup.assoc
d_assoc_2032 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2032 = erased
-- Data.Sign.Properties._.IsSemigroup.isEquivalence
d_isEquivalence_2034 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2034 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0))
-- Data.Sign.Properties._.IsSemigroup.isMagma
d_isMagma_2036 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2036 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0)
-- Data.Sign.Properties._.IsSemigroup.∙-cong
d_'8729''45'cong_2050 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2050 = erased
-- Data.Sign.Properties._.IsSemimedialMagma.isEquivalence
d_isEquivalence_2058 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0))
-- Data.Sign.Properties._.IsSemimedialMagma.isMagma
d_isMagma_2060 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2060 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0)
-- Data.Sign.Properties._.IsSemimedialMagma.semiMedial
d_semiMedial_2068 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semiMedial_2068 v0
  = coe MAlonzo.Code.Algebra.Structures.d_semiMedial_370 (coe v0)
-- Data.Sign.Properties._.IsSemimedialMagma.∙-cong
d_'8729''45'cong_2080 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2080 = erased
-- Data.Sign.Properties._.IsSemiring.*-assoc
d_'42''45'assoc_2088 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2088 = erased
-- Data.Sign.Properties._.IsSemiring.*-cong
d_'42''45'cong_2090 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2090 = erased
-- Data.Sign.Properties._.IsSemiring.*-identity
d_'42''45'identity_2096 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2096 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Sign.Properties._.IsSemiring.assoc
d_assoc_2108 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2108 = erased
-- Data.Sign.Properties._.IsSemiring.comm
d_comm_2110 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2110 = erased
-- Data.Sign.Properties._.IsSemiring.∙-cong
d_'8729''45'cong_2112 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2112 = erased
-- Data.Sign.Properties._.IsSemiring.identity
d_identity_2118 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2118 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Sign.Properties._.IsSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2126 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2126 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Sign.Properties._.IsSemiring.isMagma
d_isMagma_2130 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2130 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe v0)))))
-- Data.Sign.Properties._.IsSemiring.isMonoid
d_isMonoid_2132 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe v0)))
-- Data.Sign.Properties._.IsSemiring.isSemigroup
d_isSemigroup_2134 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2134 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Sign.Properties._.IsSemiring.distrib
d_distrib_2138 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2138 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Sign.Properties._.IsSemiring.isEquivalence
d_isEquivalence_2144 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2144 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe v0))))))
-- Data.Sign.Properties._.IsSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2150 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2150 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe v0)
-- Data.Sign.Properties._.IsSemiring.zero
d_zero_2164 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2164 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1388 (coe v0)
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.*-assoc
d_'42''45'assoc_2172 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2172 = erased
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.*-cong
d_'42''45'cong_2174 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2174 = erased
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.*-identity
d_'42''45'identity_2180 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2180 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296 (coe v0)
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.assoc
d_assoc_2192 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2192 = erased
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.comm
d_comm_2194 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2194 = erased
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.∙-cong
d_'8729''45'cong_2196 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2196 = erased
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.identity
d_identity_2202 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2202 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2210 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2210 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe v0)
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.isMagma
d_isMagma_2214 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2214 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe v0))))
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.isMonoid
d_isMonoid_2216 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2216 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v0))
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.isSemigroup
d_isSemigroup_2218 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2218 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.distrib
d_distrib_2222 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2222 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1298 (coe v0)
-- Data.Sign.Properties._.IsSemiringWithoutAnnihilatingZero.isEquivalence
d_isEquivalence_2228 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe v0)))))
-- Data.Sign.Properties._.IsSemiringWithoutOne.*-assoc
d_'42''45'assoc_2244 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2244 = erased
-- Data.Sign.Properties._.IsSemiringWithoutOne.*-cong
d_'42''45'cong_2246 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2246 = erased
-- Data.Sign.Properties._.IsSemiringWithoutOne.comm
d_comm_2256 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2256 = erased
-- Data.Sign.Properties._.IsSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2260 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2260 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe v0)
-- Data.Sign.Properties._.IsSemiringWithoutOne.isMonoid
d_isMonoid_2264 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe v0))
-- Data.Sign.Properties._.IsSemiringWithoutOne.distrib
d_distrib_2266 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2266 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1166 (coe v0)
-- Data.Sign.Properties._.IsSemiringWithoutOne.isEquivalence
d_isEquivalence_2268 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2268 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
                  (coe v0)))))
-- Data.Sign.Properties._.IsSemiringWithoutOne.zero
d_zero_2272 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2272 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1168 (coe v0)
-- Data.Sign.Properties._.IsUnitalMagma.identity
d_identity_2280 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2280 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_568 (coe v0)
-- Data.Sign.Properties._.IsUnitalMagma.isEquivalence
d_isEquivalence_2286 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2286 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0))
-- Data.Sign.Properties._.IsUnitalMagma.isMagma
d_isMagma_2288 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2288 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0)
-- Data.Sign.Properties._.IsUnitalMagma.∙-cong
d_'8729''45'cong_2302 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2302 = erased
-- Data.Sign.Properties._.Associative
d_Associative_2330 ::
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_Associative_2330 = erased
-- Data.Sign.Properties._.Cancellative
d_Cancellative_2332 ::
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_Cancellative_2332 = erased
-- Data.Sign.Properties._.Commutative
d_Commutative_2334 ::
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_Commutative_2334 = erased
-- Data.Sign.Properties._.Identity
d_Identity_2350 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_Identity_2350 = erased
-- Data.Sign.Properties._.Inverse
d_Inverse_2354 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_Inverse_2354 = erased
-- Data.Sign.Properties._.LeftCancellative
d_LeftCancellative_2364 ::
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_LeftCancellative_2364 = erased
-- Data.Sign.Properties._.LeftIdentity
d_LeftIdentity_2376 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_LeftIdentity_2376 = erased
-- Data.Sign.Properties._.RightCancellative
d_RightCancellative_2394 ::
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_RightCancellative_2394 = erased
-- Data.Sign.Properties._.RightIdentity
d_RightIdentity_2406 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  (MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
   MAlonzo.Code.Data.Sign.Base.T_Sign_6) ->
  ()
d_RightIdentity_2406 = erased
-- Data.Sign.Properties._≟_
d__'8799'__2434 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__2434 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Sign.Base.C_'45'_8
        -> case coe v1 of
             MAlonzo.Code.Data.Sign.Base.C_'45'_8
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             MAlonzo.Code.Data.Sign.Base.C_'43'_10
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Sign.Base.C_'43'_10
        -> case coe v1 of
             MAlonzo.Code.Data.Sign.Base.C_'45'_8
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Data.Sign.Base.C_'43'_10
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Sign.Properties.≡-setoid
d_'8801''45'setoid_2436 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''45'setoid_2436
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402
-- Data.Sign.Properties.≡-decSetoid
d_'8801''45'decSetoid_2438 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8801''45'decSetoid_2438
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
      (coe d__'8799'__2434)
-- Data.Sign.Properties.≡-isDecEquivalence
d_'8801''45'isDecEquivalence_2440 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_'8801''45'isDecEquivalence_2440
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isDecEquivalence_398
      (coe d__'8799'__2434)
-- Data.Sign.Properties.s≢opposite[s]
d_s'8802'opposite'91's'93'_2444 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_s'8802'opposite'91's'93'_2444 = erased
-- Data.Sign.Properties.opposite-injective
d_opposite'45'injective_2450 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_opposite'45'injective_2450 = erased
-- Data.Sign.Properties.s*s≡+
d_s'42's'8801''43'_2454 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_s'42's'8801''43'_2454 = erased
-- Data.Sign.Properties.*-identityˡ
d_'42''45'identity'737'_2456 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'737'_2456 = erased
-- Data.Sign.Properties.*-identityʳ
d_'42''45'identity'691'_2458 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'691'_2458 = erased
-- Data.Sign.Properties.*-identity
d_'42''45'identity_2460 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2460
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Sign.Properties.*-comm
d_'42''45'comm_2462 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_2462 = erased
-- Data.Sign.Properties.*-assoc
d_'42''45'assoc_2464 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2464 = erased
-- Data.Sign.Properties.*-cancelʳ-≡
d_'42''45'cancel'691''45''8801'_2466 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'691''45''8801'_2466 = erased
-- Data.Sign.Properties.*-cancelˡ-≡
d_'42''45'cancel'737''45''8801'_2472 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45''8801'_2472 = erased
-- Data.Sign.Properties.*-cancel-≡
d_'42''45'cancel'45''8801'_2478 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'cancel'45''8801'_2478
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Sign.Properties.*-inverse
d_'42''45'inverse_2480 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'inverse_2480
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Sign.Properties.*-isMagma
d_'42''45'isMagma_2482 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2482
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Sign.Properties.*-magma
d_'42''45'magma_2484 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'42''45'magma_2484
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Sign.Base.d__'42'__14 d_'42''45'isMagma_2482
-- Data.Sign.Properties.*-isSemigroup
d_'42''45'isSemigroup_2486 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2486
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'42''45'isMagma_2482) erased
-- Data.Sign.Properties.*-semigroup
d_'42''45'semigroup_2488 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'42''45'semigroup_2488
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Sign.Base.d__'42'__14 d_'42''45'isSemigroup_2486
-- Data.Sign.Properties.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_2490 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_2490
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemigroup'46'constructor_10975
      (coe d_'42''45'isSemigroup_2486) erased
-- Data.Sign.Properties.*-commutativeSemigroup
d_'42''45'commutativeSemigroup_2492 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'42''45'commutativeSemigroup_2492
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemigroup'46'constructor_10763
      MAlonzo.Code.Data.Sign.Base.d__'42'__14
      d_'42''45'isCommutativeSemigroup_2490
-- Data.Sign.Properties.*-isMonoid
d_'42''45'isMonoid_2494 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2494
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'42''45'isSemigroup_2486) (coe d_'42''45'identity_2460)
-- Data.Sign.Properties.*-monoid
d_'42''45'monoid_2496 :: MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_2496
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Sign.Base.d__'42'__14
      (coe MAlonzo.Code.Data.Sign.Base.C_'43'_10) d_'42''45'isMonoid_2494
-- Data.Sign.Properties.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_2498 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_2498
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'42''45'isMonoid_2494) erased
-- Data.Sign.Properties.*-commutativeMonoid
d_'42''45'commutativeMonoid_2500 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_2500
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Sign.Base.d__'42'__14
      (coe MAlonzo.Code.Data.Sign.Base.C_'43'_10)
      d_'42''45'isCommutativeMonoid_2498
-- Data.Sign.Properties.*-isGroup
d_'42''45'isGroup_2502 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_'42''45'isGroup_2502
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsGroup'46'constructor_22905
      (coe d_'42''45'isMonoid_2494) (coe d_'42''45'inverse_2480)
      (coe (\ v0 v1 v2 -> v2))
-- Data.Sign.Properties.*-group
d_'42''45'group_2504 :: MAlonzo.Code.Algebra.Bundles.T_Group_1266
d_'42''45'group_2504
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Group'46'constructor_21965
      MAlonzo.Code.Data.Sign.Base.d__'42'__14
      (coe MAlonzo.Code.Data.Sign.Base.C_'43'_10) (\ v0 -> v0)
      d_'42''45'isGroup_2502
-- Data.Sign.Properties.*-isAbelianGroup
d_'42''45'isAbelianGroup_2506 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'42''45'isAbelianGroup_2506
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsAbelianGroup'46'constructor_27897
      (coe d_'42''45'isGroup_2502) erased
-- Data.Sign.Properties.*-abelianGroup
d_'42''45'abelianGroup_2508 ::
  MAlonzo.Code.Algebra.Bundles.T_AbelianGroup_1378
d_'42''45'abelianGroup_2508
  = coe
      MAlonzo.Code.Algebra.Bundles.C_AbelianGroup'46'constructor_24425
      MAlonzo.Code.Data.Sign.Base.d__'42'__14
      (coe MAlonzo.Code.Data.Sign.Base.C_'43'_10) (\ v0 -> v0)
      d_'42''45'isAbelianGroup_2506
-- Data.Sign.Properties.s*opposite[s]≡-
d_s'42'opposite'91's'93''8801''45'_2512 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_s'42'opposite'91's'93''8801''45'_2512 = erased
-- Data.Sign.Properties.opposite[s]*s≡-
d_opposite'91's'93''42's'8801''45'_2516 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_opposite'91's'93''42's'8801''45'_2516 = erased
