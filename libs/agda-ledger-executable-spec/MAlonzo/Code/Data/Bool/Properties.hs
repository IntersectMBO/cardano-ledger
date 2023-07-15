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

module MAlonzo.Code.Data.Bool.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Properties.BooleanAlgebra
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.Bool.Properties._._Absorbs_
d__Absorbs__8 ::
  (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d__Absorbs__8 = erased
-- Data.Bool.Properties._._DistributesOver_
d__DistributesOver__10 ::
  (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d__DistributesOver__10 = erased
-- Data.Bool.Properties._._DistributesOverʳ_
d__DistributesOver'691'__12 ::
  (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d__DistributesOver'691'__12 = erased
-- Data.Bool.Properties._._DistributesOverˡ_
d__DistributesOver'737'__14 ::
  (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d__DistributesOver'737'__14 = erased
-- Data.Bool.Properties._._IdempotentOn_
d__IdempotentOn__16 :: (Bool -> Bool -> Bool) -> Bool -> ()
d__IdempotentOn__16 = erased
-- Data.Bool.Properties._.Absorptive
d_Absorptive_18 ::
  (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d_Absorptive_18 = erased
-- Data.Bool.Properties._.Associative
d_Associative_28 :: (Bool -> Bool -> Bool) -> ()
d_Associative_28 = erased
-- Data.Bool.Properties._.Commutative
d_Commutative_32 :: (Bool -> Bool -> Bool) -> ()
d_Commutative_32 = erased
-- Data.Bool.Properties._.Idempotent
d_Idempotent_42 :: (Bool -> Bool -> Bool) -> ()
d_Idempotent_42 = erased
-- Data.Bool.Properties._.Identity
d_Identity_48 :: Bool -> (Bool -> Bool -> Bool) -> ()
d_Identity_48 = erased
-- Data.Bool.Properties._.Inverse
d_Inverse_52 ::
  Bool -> (Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d_Inverse_52 = erased
-- Data.Bool.Properties._.Involutive
d_Involutive_56 :: (Bool -> Bool) -> ()
d_Involutive_56 = erased
-- Data.Bool.Properties._.LeftIdentity
d_LeftIdentity_74 :: Bool -> (Bool -> Bool -> Bool) -> ()
d_LeftIdentity_74 = erased
-- Data.Bool.Properties._.LeftInverse
d_LeftInverse_76 ::
  Bool -> (Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d_LeftInverse_76 = erased
-- Data.Bool.Properties._.LeftZero
d_LeftZero_82 :: Bool -> (Bool -> Bool -> Bool) -> ()
d_LeftZero_82 = erased
-- Data.Bool.Properties._.RightIdentity
d_RightIdentity_104 :: Bool -> (Bool -> Bool -> Bool) -> ()
d_RightIdentity_104 = erased
-- Data.Bool.Properties._.RightInverse
d_RightInverse_106 ::
  Bool -> (Bool -> Bool) -> (Bool -> Bool -> Bool) -> ()
d_RightInverse_106 = erased
-- Data.Bool.Properties._.RightZero
d_RightZero_112 :: Bool -> (Bool -> Bool -> Bool) -> ()
d_RightZero_112 = erased
-- Data.Bool.Properties._.Selective
d_Selective_114 :: (Bool -> Bool -> Bool) -> ()
d_Selective_114 = erased
-- Data.Bool.Properties._.Zero
d_Zero_130 :: Bool -> (Bool -> Bool -> Bool) -> ()
d_Zero_130 = erased
-- Data.Bool.Properties._.IsAbelianGroup
d_IsAbelianGroup_134 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsAlternativeMagma
d_IsAlternativeMagma_136 a0 = ()
-- Data.Bool.Properties._.IsBand
d_IsBand_138 a0 = ()
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring
d_IsCancellativeCommutativeSemiring_140 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsCommutativeMagma
d_IsCommutativeMagma_142 a0 = ()
-- Data.Bool.Properties._.IsCommutativeMonoid
d_IsCommutativeMonoid_144 a0 a1 = ()
-- Data.Bool.Properties._.IsCommutativeRing
d_IsCommutativeRing_146 a0 a1 a2 a3 a4 = ()
-- Data.Bool.Properties._.IsCommutativeSemigroup
d_IsCommutativeSemigroup_148 a0 = ()
-- Data.Bool.Properties._.IsCommutativeSemiring
d_IsCommutativeSemiring_150 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne
d_IsCommutativeSemiringWithoutOne_152 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsFlexibleMagma
d_IsFlexibleMagma_154 a0 = ()
-- Data.Bool.Properties._.IsGroup
d_IsGroup_156 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid
d_IsIdempotentCommutativeMonoid_158 a0 a1 = ()
-- Data.Bool.Properties._.IsIdempotentMagma
d_IsIdempotentMagma_160 a0 = ()
-- Data.Bool.Properties._.IsIdempotentSemiring
d_IsIdempotentSemiring_162 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsInvertibleMagma
d_IsInvertibleMagma_164 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsInvertibleUnitalMagma
d_IsInvertibleUnitalMagma_166 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsKleeneAlgebra
d_IsKleeneAlgebra_168 a0 a1 a2 a3 a4 = ()
-- Data.Bool.Properties._.IsLeftBolLoop
d_IsLeftBolLoop_170 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsLoop
d_IsLoop_172 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsMagma
d_IsMagma_174 a0 = ()
-- Data.Bool.Properties._.IsMedialMagma
d_IsMedialMagma_176 a0 = ()
-- Data.Bool.Properties._.IsMiddleBolLoop
d_IsMiddleBolLoop_178 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsMonoid
d_IsMonoid_180 a0 a1 = ()
-- Data.Bool.Properties._.IsMoufangLoop
d_IsMoufangLoop_182 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsNearSemiring
d_IsNearSemiring_184 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsNearring
d_IsNearring_186 a0 a1 a2 a3 a4 = ()
-- Data.Bool.Properties._.IsNonAssociativeRing
d_IsNonAssociativeRing_188 a0 a1 a2 a3 a4 = ()
-- Data.Bool.Properties._.IsQuasigroup
d_IsQuasigroup_190 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsQuasiring
d_IsQuasiring_192 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsRightBolLoop
d_IsRightBolLoop_194 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsRing
d_IsRing_196 a0 a1 a2 a3 a4 = ()
-- Data.Bool.Properties._.IsRingWithoutOne
d_IsRingWithoutOne_198 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsSelectiveMagma
d_IsSelectiveMagma_200 a0 = ()
-- Data.Bool.Properties._.IsSemigroup
d_IsSemigroup_202 a0 = ()
-- Data.Bool.Properties._.IsSemimedialMagma
d_IsSemimedialMagma_204 a0 = ()
-- Data.Bool.Properties._.IsSemiring
d_IsSemiring_206 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero
d_IsSemiringWithoutAnnihilatingZero_208 a0 a1 a2 a3 = ()
-- Data.Bool.Properties._.IsSemiringWithoutOne
d_IsSemiringWithoutOne_210 a0 a1 a2 = ()
-- Data.Bool.Properties._.IsUnitalMagma
d_IsUnitalMagma_212 a0 a1 = ()
-- Data.Bool.Properties._.IsAbelianGroup.assoc
d_assoc_218 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_218 = erased
-- Data.Bool.Properties._.IsAbelianGroup.comm
d_comm_220 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_220 = erased
-- Data.Bool.Properties._.IsAbelianGroup.identity
d_identity_222 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Bool.Properties._.IsAbelianGroup.inverse
d_inverse_228 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Bool.Properties._.IsAbelianGroup.isEquivalence
d_isEquivalence_240 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))))
-- Data.Bool.Properties._.IsAbelianGroup.isGroup
d_isGroup_242 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_242 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)
-- Data.Bool.Properties._.IsAbelianGroup.isMagma
d_isMagma_248 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_248 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))))
-- Data.Bool.Properties._.IsAbelianGroup.isMonoid
d_isMonoid_250 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_250 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Bool.Properties._.IsAbelianGroup.isSemigroup
d_isSemigroup_254 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Bool.Properties._.IsAbelianGroup.⁻¹-cong
d_'8315''185''45'cong_272 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_272 = erased
-- Data.Bool.Properties._.IsAbelianGroup.∙-cong
d_'8729''45'cong_274 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_274 = erased
-- Data.Bool.Properties._.IsAlternativeMagma.alter
d_alter_282 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alter_282 v0
  = coe MAlonzo.Code.Algebra.Structures.d_alter_258 (coe v0)
-- Data.Bool.Properties._.IsAlternativeMagma.isEquivalence
d_isEquivalence_288 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_288 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0))
-- Data.Bool.Properties._.IsAlternativeMagma.isMagma
d_isMagma_290 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_290 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0)
-- Data.Bool.Properties._.IsAlternativeMagma.∙-cong
d_'8729''45'cong_304 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_304 = erased
-- Data.Bool.Properties._.IsBand.assoc
d_assoc_312 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_312 = erased
-- Data.Bool.Properties._.IsBand.idem
d_idem_314 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_314 = erased
-- Data.Bool.Properties._.IsBand.isEquivalence
d_isEquivalence_316 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_316 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)))
-- Data.Bool.Properties._.IsBand.isMagma
d_isMagma_318 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_318 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0))
-- Data.Bool.Properties._.IsBand.isSemigroup
d_isSemigroup_322 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_322 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)
-- Data.Bool.Properties._.IsBand.∙-cong
d_'8729''45'cong_334 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_334 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.*-assoc
d_'42''45'assoc_342 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_342 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.*-cancelˡ-nonZero
d_'42''45'cancel'737''45'nonZero_344 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Bool ->
  Bool ->
  Bool ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45'nonZero_344 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.*-comm
d_'42''45'comm_346 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_346 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.*-cong
d_'42''45'cong_348 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_348 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.*-identity
d_'42''45'identity_354 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_354 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.assoc
d_assoc_372 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_372 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.comm
d_comm_374 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_374 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.∙-cong
d_'8729''45'cong_376 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_376 = erased
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.identity
d_identity_382 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_382 v0
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
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_390 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_390 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.isMagma
d_isMagma_394 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_394 v0
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
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.isMonoid
d_isMonoid_396 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_396 v0
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
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.isSemigroup
d_isSemigroup_398 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_398 v0
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
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.distrib
d_distrib_402 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_402 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe v0))))
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.isCommutativeSemiring
d_isCommutativeSemiring_408 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_408 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
      (coe v0)
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.isEquivalence
d_isEquivalence_412 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_412 v0
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
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.isSemiring
d_isSemiring_418 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe v0))
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_420 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_420 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Bool.Properties._.IsCancellativeCommutativeSemiring.zero
d_zero_434 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_434 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe v0)))
-- Data.Bool.Properties._.IsCommutativeMagma.comm
d_comm_442 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_442 = erased
-- Data.Bool.Properties._.IsCommutativeMagma.isEquivalence
d_isEquivalence_444 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_444 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0))
-- Data.Bool.Properties._.IsCommutativeMagma.isMagma
d_isMagma_446 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_446 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0)
-- Data.Bool.Properties._.IsCommutativeMagma.∙-cong
d_'8729''45'cong_460 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_460 = erased
-- Data.Bool.Properties._.IsCommutativeMonoid.assoc
d_assoc_468 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_468 = erased
-- Data.Bool.Properties._.IsCommutativeMonoid.comm
d_comm_470 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_470 = erased
-- Data.Bool.Properties._.IsCommutativeMonoid.identity
d_identity_472 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_472 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Bool.Properties._.IsCommutativeMonoid.isEquivalence
d_isEquivalence_482 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_482 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))))
-- Data.Bool.Properties._.IsCommutativeMonoid.isMagma
d_isMagma_484 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_484 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)))
-- Data.Bool.Properties._.IsCommutativeMonoid.isMonoid
d_isMonoid_486 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_486 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)
-- Data.Bool.Properties._.IsCommutativeMonoid.isSemigroup
d_isSemigroup_490 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_490 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Bool.Properties._.IsCommutativeMonoid.∙-cong
d_'8729''45'cong_504 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_504 = erased
-- Data.Bool.Properties._.IsCommutativeRing.*-assoc
d_'42''45'assoc_514 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_514 = erased
-- Data.Bool.Properties._.IsCommutativeRing.*-comm
d_'42''45'comm_516 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_516 = erased
-- Data.Bool.Properties._.IsCommutativeRing.*-cong
d_'42''45'cong_518 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_518 = erased
-- Data.Bool.Properties._.IsCommutativeRing.*-identity
d_'42''45'identity_524 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_524 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Bool.Properties._.IsCommutativeRing.assoc
d_assoc_542 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_542 = erased
-- Data.Bool.Properties._.IsCommutativeRing.comm
d_comm_544 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_544 = erased
-- Data.Bool.Properties._.IsCommutativeRing.∙-cong
d_'8729''45'cong_546 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_546 = erased
-- Data.Bool.Properties._.IsCommutativeRing.identity
d_identity_552 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_552 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Bool.Properties._.IsCommutativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_558 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_558 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Bool.Properties._.IsCommutativeRing.isGroup
d_isGroup_566 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_566 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))
-- Data.Bool.Properties._.IsCommutativeRing.isMagma
d_isMagma_572 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_572 v0
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
-- Data.Bool.Properties._.IsCommutativeRing.isMonoid
d_isMonoid_574 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Bool.Properties._.IsCommutativeRing.isSemigroup
d_isSemigroup_576 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_576 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))))
-- Data.Bool.Properties._.IsCommutativeRing.⁻¹-cong
d_'8315''185''45'cong_580 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_580 = erased
-- Data.Bool.Properties._.IsCommutativeRing.inverse
d_inverse_582 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))))
-- Data.Bool.Properties._.IsCommutativeRing.distrib
d_distrib_588 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_588 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2426
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Bool.Properties._.IsCommutativeRing.isEquivalence
d_isEquivalence_598 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_598 v0
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
-- Data.Bool.Properties._.IsCommutativeRing.isRing
d_isRing_604 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_604 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)
-- Data.Bool.Properties._.IsCommutativeRing.zero
d_zero_626 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_626 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2428
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Bool.Properties._.IsCommutativeSemigroup.assoc
d_assoc_634 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_634 = erased
-- Data.Bool.Properties._.IsCommutativeSemigroup.comm
d_comm_636 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_636 = erased
-- Data.Bool.Properties._.IsCommutativeSemigroup.isEquivalence
d_isEquivalence_640 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_640 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)))
-- Data.Bool.Properties._.IsCommutativeSemigroup.isMagma
d_isMagma_642 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0))
-- Data.Bool.Properties._.IsCommutativeSemigroup.isSemigroup
d_isSemigroup_646 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_646 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)
-- Data.Bool.Properties._.IsCommutativeSemigroup.∙-cong
d_'8729''45'cong_658 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_658 = erased
-- Data.Bool.Properties._.IsCommutativeSemiring.*-assoc
d_'42''45'assoc_666 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_666 = erased
-- Data.Bool.Properties._.IsCommutativeSemiring.*-comm
d_'42''45'comm_668 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_668 = erased
-- Data.Bool.Properties._.IsCommutativeSemiring.*-cong
d_'42''45'cong_670 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_670 = erased
-- Data.Bool.Properties._.IsCommutativeSemiring.*-identity
d_'42''45'identity_676 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_676 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Bool.Properties._.IsCommutativeSemiring.assoc
d_assoc_694 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_694 = erased
-- Data.Bool.Properties._.IsCommutativeSemiring.comm
d_comm_696 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_696 = erased
-- Data.Bool.Properties._.IsCommutativeSemiring.∙-cong
d_'8729''45'cong_698 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_698 = erased
-- Data.Bool.Properties._.IsCommutativeSemiring.identity
d_identity_704 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_704 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Bool.Properties._.IsCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_712 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_712 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Bool.Properties._.IsCommutativeSemiring.isMagma
d_isMagma_716 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_716 v0
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
-- Data.Bool.Properties._.IsCommutativeSemiring.isMonoid
d_isMonoid_718 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))))
-- Data.Bool.Properties._.IsCommutativeSemiring.isSemigroup
d_isSemigroup_720 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_720 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))))
-- Data.Bool.Properties._.IsCommutativeSemiring.distrib
d_distrib_724 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_724 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Bool.Properties._.IsCommutativeSemiring.isEquivalence
d_isEquivalence_732 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_732 v0
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
-- Data.Bool.Properties._.IsCommutativeSemiring.isSemiring
d_isSemiring_738 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_738 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)
-- Data.Bool.Properties._.IsCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_740 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_740 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Bool.Properties._.IsCommutativeSemiring.zero
d_zero_754 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_754 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.*-assoc
d_'42''45'assoc_762 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_762 = erased
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.*-comm
d_'42''45'comm_764 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_764 = erased
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.*-cong
d_'42''45'cong_766 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_766 = erased
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.comm
d_comm_780 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_780 = erased
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_784 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_784 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.isMonoid
d_isMonoid_788 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_788 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
            (coe v0)))
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.distrib
d_distrib_790 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_790 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1166
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.isEquivalence
d_isEquivalence_792 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_792 v0
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
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.isSemiringWithoutOne
d_isSemiringWithoutOne_796 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_796 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
      (coe v0)
-- Data.Bool.Properties._.IsCommutativeSemiringWithoutOne.zero
d_zero_798 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_798 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1168
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Bool.Properties._.IsFlexibleMagma.flex
d_flex_806 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_flex_806 = erased
-- Data.Bool.Properties._.IsFlexibleMagma.isEquivalence
d_isEquivalence_808 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_808 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0))
-- Data.Bool.Properties._.IsFlexibleMagma.isMagma
d_isMagma_810 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_810 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0)
-- Data.Bool.Properties._.IsFlexibleMagma.∙-cong
d_'8729''45'cong_824 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_824 = erased
-- Data.Bool.Properties._.IsGroup.assoc
d_assoc_834 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_834 = erased
-- Data.Bool.Properties._.IsGroup.identity
d_identity_836 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_836 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Bool.Properties._.IsGroup.inverse
d_inverse_842 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_842 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_904 (coe v0)
-- Data.Bool.Properties._.IsGroup.isEquivalence
d_isEquivalence_848 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))))
-- Data.Bool.Properties._.IsGroup.isMagma
d_isMagma_854 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_854 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)))
-- Data.Bool.Properties._.IsGroup.isMonoid
d_isMonoid_856 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_856 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)
-- Data.Bool.Properties._.IsGroup.isSemigroup
d_isSemigroup_860 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_860 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Bool.Properties._.IsGroup.⁻¹-cong
d_'8315''185''45'cong_878 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_878 = erased
-- Data.Bool.Properties._.IsGroup.∙-cong
d_'8729''45'cong_880 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_880 = erased
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.assoc
d_assoc_888 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_888 = erased
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.comm
d_comm_890 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_890 = erased
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.idem
d_idem_892 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_892 = erased
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.identity
d_identity_894 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.isCommutativeMonoid
d_isCommutativeMonoid_904 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_904 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0)
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.isEquivalence
d_isEquivalence_908 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_908 v0
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
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.isMagma
d_isMagma_910 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_910 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.isMonoid
d_isMonoid_912 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_912 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.isSemigroup
d_isSemigroup_916 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_916 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsIdempotentCommutativeMonoid.∙-cong
d_'8729''45'cong_930 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_930 = erased
-- Data.Bool.Properties._.IsIdempotentMagma.idem
d_idem_938 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_938 = erased
-- Data.Bool.Properties._.IsIdempotentMagma.isEquivalence
d_isEquivalence_940 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_940 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0))
-- Data.Bool.Properties._.IsIdempotentMagma.isMagma
d_isMagma_942 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_942 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0)
-- Data.Bool.Properties._.IsIdempotentMagma.∙-cong
d_'8729''45'cong_956 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_956 = erased
-- Data.Bool.Properties._.IsIdempotentSemiring.*-assoc
d_'42''45'assoc_964 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_964 = erased
-- Data.Bool.Properties._.IsIdempotentSemiring.*-cong
d_'42''45'cong_966 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_966 = erased
-- Data.Bool.Properties._.IsIdempotentSemiring.*-identity
d_'42''45'identity_972 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_972 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Bool.Properties._.IsIdempotentSemiring.assoc
d_assoc_984 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_984 = erased
-- Data.Bool.Properties._.IsIdempotentSemiring.comm
d_comm_986 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_986 = erased
-- Data.Bool.Properties._.IsIdempotentSemiring.∙-cong
d_'8729''45'cong_988 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_988 = erased
-- Data.Bool.Properties._.IsIdempotentSemiring.+-idem
d_'43''45'idem_994 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_994 = erased
-- Data.Bool.Properties._.IsIdempotentSemiring.identity
d_identity_996 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_996 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Bool.Properties._.IsIdempotentSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1004 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1004 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Bool.Properties._.IsIdempotentSemiring.isMagma
d_isMagma_1008 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1008 v0
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
-- Data.Bool.Properties._.IsIdempotentSemiring.isMonoid
d_isMonoid_1010 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1010 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))))
-- Data.Bool.Properties._.IsIdempotentSemiring.isSemigroup
d_isSemigroup_1012 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1012 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))))
-- Data.Bool.Properties._.IsIdempotentSemiring.distrib
d_distrib_1016 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1016 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Bool.Properties._.IsIdempotentSemiring.isEquivalence
d_isEquivalence_1022 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1022 v0
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
-- Data.Bool.Properties._.IsIdempotentSemiring.isSemiring
d_isSemiring_1028 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1028 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)
-- Data.Bool.Properties._.IsIdempotentSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1030 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1030 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Bool.Properties._.IsIdempotentSemiring.zero
d_zero_1044 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Bool.Properties._.IsInvertibleMagma.inverse
d_inverse_1052 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1052 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_792 (coe v0)
-- Data.Bool.Properties._.IsInvertibleMagma.isEquivalence
d_isEquivalence_1058 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0))
-- Data.Bool.Properties._.IsInvertibleMagma.isMagma
d_isMagma_1060 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1060 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0)
-- Data.Bool.Properties._.IsInvertibleMagma.⁻¹-cong
d_'8315''185''45'cong_1074 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1074 = erased
-- Data.Bool.Properties._.IsInvertibleMagma.∙-cong
d_'8729''45'cong_1076 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1076 = erased
-- Data.Bool.Properties._.IsInvertibleUnitalMagma.identity
d_identity_1084 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1084 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_842 (coe v0)
-- Data.Bool.Properties._.IsInvertibleUnitalMagma.inverse
d_inverse_1090 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1090 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_792
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Bool.Properties._.IsInvertibleUnitalMagma.isEquivalence
d_isEquivalence_1096 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1096 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe
            MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)))
-- Data.Bool.Properties._.IsInvertibleUnitalMagma.isInvertibleMagma
d_isInvertibleMagma_1098 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1098 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)
-- Data.Bool.Properties._.IsInvertibleUnitalMagma.isMagma
d_isMagma_1100 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1100 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_790
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Bool.Properties._.IsInvertibleUnitalMagma.⁻¹-cong
d_'8315''185''45'cong_1116 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1116 = erased
-- Data.Bool.Properties._.IsInvertibleUnitalMagma.∙-cong
d_'8729''45'cong_1118 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1118 = erased
-- Data.Bool.Properties._.IsKleeneAlgebra.*-assoc
d_'42''45'assoc_1126 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1126 = erased
-- Data.Bool.Properties._.IsKleeneAlgebra.*-cong
d_'42''45'cong_1128 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1128 = erased
-- Data.Bool.Properties._.IsKleeneAlgebra.*-identity
d_'42''45'identity_1134 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1134 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Bool.Properties._.IsKleeneAlgebra.assoc
d_assoc_1146 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1146 = erased
-- Data.Bool.Properties._.IsKleeneAlgebra.comm
d_comm_1148 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1148 = erased
-- Data.Bool.Properties._.IsKleeneAlgebra.∙-cong
d_'8729''45'cong_1150 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1150 = erased
-- Data.Bool.Properties._.IsKleeneAlgebra.+-idem
d_'43''45'idem_1156 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_1156 = erased
-- Data.Bool.Properties._.IsKleeneAlgebra.identity
d_identity_1158 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1158 v0
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
-- Data.Bool.Properties._.IsKleeneAlgebra.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1166 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1166 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Bool.Properties._.IsKleeneAlgebra.isMagma
d_isMagma_1170 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1170 v0
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
-- Data.Bool.Properties._.IsKleeneAlgebra.isMonoid
d_isMonoid_1172 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1172 v0
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
-- Data.Bool.Properties._.IsKleeneAlgebra.isSemigroup
d_isSemigroup_1174 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1174 v0
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
-- Data.Bool.Properties._.IsKleeneAlgebra.distrib
d_distrib_1178 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1178 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe v0))))
-- Data.Bool.Properties._.IsKleeneAlgebra.isEquivalence
d_isEquivalence_1184 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1184 v0
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
-- Data.Bool.Properties._.IsKleeneAlgebra.isIdempotentSemiring
d_isIdempotentSemiring_1186 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_1186 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
      (coe v0)
-- Data.Bool.Properties._.IsKleeneAlgebra.isSemiring
d_isSemiring_1192 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1192 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
      (coe
         MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe v0))
-- Data.Bool.Properties._.IsKleeneAlgebra.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1194 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1194 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Bool.Properties._.IsKleeneAlgebra.starDestructive
d_starDestructive_1204 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starDestructive_1204 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_starDestructive_1856 (coe v0)
-- Data.Bool.Properties._.IsKleeneAlgebra.starExpansive
d_starExpansive_1210 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starExpansive_1210 v0
  = coe MAlonzo.Code.Algebra.Structures.d_starExpansive_1854 (coe v0)
-- Data.Bool.Properties._.IsKleeneAlgebra.zero
d_zero_1220 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1220 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe v0)))
-- Data.Bool.Properties._.IsLeftBolLoop.//-cong
d_'47''47''45'cong_1228 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1228 = erased
-- Data.Bool.Properties._.IsLeftBolLoop.\\-cong
d_'92''92''45'cong_1234 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1234 = erased
-- Data.Bool.Properties._.IsLeftBolLoop.identity
d_identity_1240 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Bool.Properties._.IsLeftBolLoop.isEquivalence
d_isEquivalence_1246 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1246 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))))
-- Data.Bool.Properties._.IsLeftBolLoop.isLoop
d_isLoop_1248 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1248 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)
-- Data.Bool.Properties._.IsLeftBolLoop.isMagma
d_isMagma_1250 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1250 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Bool.Properties._.IsLeftBolLoop.isQuasigroup
d_isQuasigroup_1254 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Bool.Properties._.IsLeftBolLoop.leftBol
d_leftBol_1256 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1256 = erased
-- Data.Bool.Properties._.IsLeftBolLoop.leftDivides
d_leftDivides_1258 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1258 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Bool.Properties._.IsLeftBolLoop.rightDivides
d_rightDivides_1268 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1268 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Bool.Properties._.IsLeftBolLoop.∙-cong
d_'8729''45'cong_1280 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1280 = erased
-- Data.Bool.Properties._.IsLoop.//-cong
d_'47''47''45'cong_1288 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1288 = erased
-- Data.Bool.Properties._.IsLoop.\\-cong
d_'92''92''45'cong_1294 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1294 = erased
-- Data.Bool.Properties._.IsLoop.identity
d_identity_1300 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1300 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2784 (coe v0)
-- Data.Bool.Properties._.IsLoop.isEquivalence
d_isEquivalence_1306 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)))
-- Data.Bool.Properties._.IsLoop.isMagma
d_isMagma_1308 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1308 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Bool.Properties._.IsLoop.isQuasigroup
d_isQuasigroup_1312 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1312 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)
-- Data.Bool.Properties._.IsLoop.leftDivides
d_leftDivides_1314 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1314 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Bool.Properties._.IsLoop.rightDivides
d_rightDivides_1324 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1324 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Bool.Properties._.IsLoop.∙-cong
d_'8729''45'cong_1336 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1336 = erased
-- Data.Bool.Properties._.IsMagma.isEquivalence
d_isEquivalence_1344 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1344 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v0)
-- Data.Bool.Properties._.IsMagma.∙-cong
d_'8729''45'cong_1358 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1358 = erased
-- Data.Bool.Properties._.IsMedialMagma.isEquivalence
d_isEquivalence_1366 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1366 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0))
-- Data.Bool.Properties._.IsMedialMagma.isMagma
d_isMagma_1368 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1368 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0)
-- Data.Bool.Properties._.IsMedialMagma.medial
d_medial_1372 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  Bool ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_medial_1372 = erased
-- Data.Bool.Properties._.IsMedialMagma.∙-cong
d_'8729''45'cong_1384 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1384 = erased
-- Data.Bool.Properties._.IsMiddleBolLoop.//-cong
d_'47''47''45'cong_1392 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1392 = erased
-- Data.Bool.Properties._.IsMiddleBolLoop.\\-cong
d_'92''92''45'cong_1398 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1398 = erased
-- Data.Bool.Properties._.IsMiddleBolLoop.identity
d_identity_1404 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1404 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Bool.Properties._.IsMiddleBolLoop.isEquivalence
d_isEquivalence_1410 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1410 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))))
-- Data.Bool.Properties._.IsMiddleBolLoop.isLoop
d_isLoop_1412 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1412 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)
-- Data.Bool.Properties._.IsMiddleBolLoop.isMagma
d_isMagma_1414 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1414 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Bool.Properties._.IsMiddleBolLoop.isQuasigroup
d_isQuasigroup_1418 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Bool.Properties._.IsMiddleBolLoop.leftDivides
d_leftDivides_1420 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1420 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Bool.Properties._.IsMiddleBolLoop.middleBol
d_middleBol_1426 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_middleBol_1426 = erased
-- Data.Bool.Properties._.IsMiddleBolLoop.rightDivides
d_rightDivides_1432 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1432 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Bool.Properties._.IsMiddleBolLoop.∙-cong
d_'8729''45'cong_1444 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1444 = erased
-- Data.Bool.Properties._.IsMonoid.assoc
d_assoc_1452 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1452 = erased
-- Data.Bool.Properties._.IsMonoid.identity
d_identity_1454 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1454 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_612 (coe v0)
-- Data.Bool.Properties._.IsMonoid.isEquivalence
d_isEquivalence_1460 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1460 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)))
-- Data.Bool.Properties._.IsMonoid.isMagma
d_isMagma_1462 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1462 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0))
-- Data.Bool.Properties._.IsMonoid.isSemigroup
d_isSemigroup_1466 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1466 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)
-- Data.Bool.Properties._.IsMonoid.∙-cong
d_'8729''45'cong_1480 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1480 = erased
-- Data.Bool.Properties._.IsMoufangLoop.//-cong
d_'47''47''45'cong_1488 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1488 = erased
-- Data.Bool.Properties._.IsMoufangLoop.\\-cong
d_'92''92''45'cong_1494 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1494 = erased
-- Data.Bool.Properties._.IsMoufangLoop.identical
d_identical_1500 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identical_1500 = erased
-- Data.Bool.Properties._.IsMoufangLoop.identity
d_identity_1502 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1502 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Bool.Properties._.IsMoufangLoop.isEquivalence
d_isEquivalence_1508 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1508 v0
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
-- Data.Bool.Properties._.IsMoufangLoop.isLeftBolLoop
d_isLeftBolLoop_1510 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
d_isLeftBolLoop_1510 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)
-- Data.Bool.Properties._.IsMoufangLoop.isLoop
d_isLoop_1512 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1512 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2860
      (coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))
-- Data.Bool.Properties._.IsMoufangLoop.isMagma
d_isMagma_1514 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1514 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Bool.Properties._.IsMoufangLoop.isQuasigroup
d_isQuasigroup_1518 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1518 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)))
-- Data.Bool.Properties._.IsMoufangLoop.leftBol
d_leftBol_1520 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1520 = erased
-- Data.Bool.Properties._.IsMoufangLoop.leftDivides
d_leftDivides_1522 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1522 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Bool.Properties._.IsMoufangLoop.rightBol
d_rightBol_1532 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1532 = erased
-- Data.Bool.Properties._.IsMoufangLoop.rightDivides
d_rightDivides_1534 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1534 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))))
-- Data.Bool.Properties._.IsMoufangLoop.∙-cong
d_'8729''45'cong_1546 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1546 = erased
-- Data.Bool.Properties._.IsNearSemiring.*-assoc
d_'42''45'assoc_1554 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1554 = erased
-- Data.Bool.Properties._.IsNearSemiring.*-cong
d_'42''45'cong_1556 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1556 = erased
-- Data.Bool.Properties._.IsNearSemiring.assoc
d_assoc_1566 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1566 = erased
-- Data.Bool.Properties._.IsNearSemiring.∙-cong
d_'8729''45'cong_1568 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1568 = erased
-- Data.Bool.Properties._.IsNearSemiring.identity
d_identity_1574 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Bool.Properties._.IsNearSemiring.isMagma
d_isMagma_1580 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1580 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)))
-- Data.Bool.Properties._.IsNearSemiring.+-isMonoid
d_'43''45'isMonoid_1582 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)
-- Data.Bool.Properties._.IsNearSemiring.isSemigroup
d_isSemigroup_1584 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1584 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Bool.Properties._.IsNearSemiring.distribʳ
d_distrib'691'_1588 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691'_1588 = erased
-- Data.Bool.Properties._.IsNearSemiring.isEquivalence
d_isEquivalence_1590 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1590 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))))
-- Data.Bool.Properties._.IsNearSemiring.zeroˡ
d_zero'737'_1604 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zero'737'_1604 = erased
-- Data.Bool.Properties._.IsNearring.*-assoc
d_'42''45'assoc_1608 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1608 = erased
-- Data.Bool.Properties._.IsNearring.*-cong
d_'42''45'cong_1610 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1610 = erased
-- Data.Bool.Properties._.IsNearring.*-identity
d_'42''45'identity_1616 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1616 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Bool.Properties._.IsNearring.assoc
d_assoc_1628 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1628 = erased
-- Data.Bool.Properties._.IsNearring.∙-cong
d_'8729''45'cong_1630 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1630 = erased
-- Data.Bool.Properties._.IsNearring.identity
d_identity_1636 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1636 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Bool.Properties._.IsNearring.+-inverse
d_'43''45'inverse_1642 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_1642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'inverse_2314 (coe v0)
-- Data.Bool.Properties._.IsNearring.isMagma
d_isMagma_1648 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1648 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))))
-- Data.Bool.Properties._.IsNearring.+-isMonoid
d_'43''45'isMonoid_1650 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1650 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Bool.Properties._.IsNearring.isSemigroup
d_isSemigroup_1652 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1652 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Bool.Properties._.IsNearring.distrib
d_distrib_1656 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1656 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1992
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Bool.Properties._.IsNearring.isEquivalence
d_isEquivalence_1658 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1658 v0
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
-- Data.Bool.Properties._.IsNearring.isQuasiring
d_isQuasiring_1662 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
d_isQuasiring_1662 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)
-- Data.Bool.Properties._.IsNearring.zero
d_zero_1674 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1674 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1994
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Bool.Properties._.IsNearring.⁻¹-cong
d_'8315''185''45'cong_1676 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1676 = erased
-- Data.Bool.Properties._.IsNonAssociativeRing.*-cong
d_'42''45'cong_1682 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1682 = erased
-- Data.Bool.Properties._.IsNonAssociativeRing.assoc
d_assoc_1690 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1690 = erased
-- Data.Bool.Properties._.IsNonAssociativeRing.comm
d_comm_1692 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1692 = erased
-- Data.Bool.Properties._.IsNonAssociativeRing.∙-cong
d_'8729''45'cong_1694 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1694 = erased
-- Data.Bool.Properties._.IsNonAssociativeRing.identity
d_identity_1700 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1700 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Bool.Properties._.IsNonAssociativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1706 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1706 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
      (coe v0)
-- Data.Bool.Properties._.IsNonAssociativeRing.isGroup
d_isGroup_1714 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1714 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe v0))
-- Data.Bool.Properties._.IsNonAssociativeRing.isMagma
d_isMagma_1720 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1720 v0
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
-- Data.Bool.Properties._.IsNonAssociativeRing.isMonoid
d_isMonoid_1722 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1722 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Bool.Properties._.IsNonAssociativeRing.isSemigroup
d_isSemigroup_1724 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1724 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe v0))))
-- Data.Bool.Properties._.IsNonAssociativeRing.⁻¹-cong
d_'8315''185''45'cong_1728 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1728 = erased
-- Data.Bool.Properties._.IsNonAssociativeRing.inverse
d_inverse_1730 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1730 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe v0)))
-- Data.Bool.Properties._.IsNonAssociativeRing.distrib
d_distrib_1736 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1736 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2208 (coe v0)
-- Data.Bool.Properties._.IsNonAssociativeRing.identity
d_identity_1738 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1738 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2206 (coe v0)
-- Data.Bool.Properties._.IsNonAssociativeRing.isEquivalence
d_isEquivalence_1740 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1740 v0
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
-- Data.Bool.Properties._.IsNonAssociativeRing.zero
d_zero_1758 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1758 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2210 (coe v0)
-- Data.Bool.Properties._.IsQuasigroup.//-cong
d_'47''47''45'cong_1762 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1762 = erased
-- Data.Bool.Properties._.IsQuasigroup.\\-cong
d_'92''92''45'cong_1768 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1768 = erased
-- Data.Bool.Properties._.IsQuasigroup.isEquivalence
d_isEquivalence_1774 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1774 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0))
-- Data.Bool.Properties._.IsQuasigroup.isMagma
d_isMagma_1776 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1776 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0)
-- Data.Bool.Properties._.IsQuasigroup.leftDivides
d_leftDivides_1780 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1780 v0
  = coe MAlonzo.Code.Algebra.Structures.d_leftDivides_2710 (coe v0)
-- Data.Bool.Properties._.IsQuasigroup.rightDivides
d_rightDivides_1790 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1790 v0
  = coe MAlonzo.Code.Algebra.Structures.d_rightDivides_2712 (coe v0)
-- Data.Bool.Properties._.IsQuasigroup.∙-cong
d_'8729''45'cong_1802 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1802 = erased
-- Data.Bool.Properties._.IsQuasiring.*-assoc
d_'42''45'assoc_1810 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1810 = erased
-- Data.Bool.Properties._.IsQuasiring.*-cong
d_'42''45'cong_1812 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1812 = erased
-- Data.Bool.Properties._.IsQuasiring.*-identity
d_'42''45'identity_1818 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1818 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990 (coe v0)
-- Data.Bool.Properties._.IsQuasiring.assoc
d_assoc_1830 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1830 = erased
-- Data.Bool.Properties._.IsQuasiring.∙-cong
d_'8729''45'cong_1832 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1832 = erased
-- Data.Bool.Properties._.IsQuasiring.identity
d_identity_1838 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1838 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Bool.Properties._.IsQuasiring.isMagma
d_isMagma_1844 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1844 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)))
-- Data.Bool.Properties._.IsQuasiring.+-isMonoid
d_'43''45'isMonoid_1846 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1846 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)
-- Data.Bool.Properties._.IsQuasiring.isSemigroup
d_isSemigroup_1848 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Bool.Properties._.IsQuasiring.distrib
d_distrib_1852 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1852 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1992 (coe v0)
-- Data.Bool.Properties._.IsQuasiring.isEquivalence
d_isEquivalence_1854 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1854 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))))
-- Data.Bool.Properties._.IsQuasiring.zero
d_zero_1868 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1868 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1994 (coe v0)
-- Data.Bool.Properties._.IsRightBolLoop.//-cong
d_'47''47''45'cong_1872 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1872 = erased
-- Data.Bool.Properties._.IsRightBolLoop.\\-cong
d_'92''92''45'cong_1878 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1878 = erased
-- Data.Bool.Properties._.IsRightBolLoop.identity
d_identity_1884 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1884 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Bool.Properties._.IsRightBolLoop.isEquivalence
d_isEquivalence_1890 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1890 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))))
-- Data.Bool.Properties._.IsRightBolLoop.isLoop
d_isLoop_1892 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1892 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)
-- Data.Bool.Properties._.IsRightBolLoop.isMagma
d_isMagma_1894 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Bool.Properties._.IsRightBolLoop.isQuasigroup
d_isQuasigroup_1898 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1898 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Bool.Properties._.IsRightBolLoop.leftDivides
d_leftDivides_1900 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1900 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Bool.Properties._.IsRightBolLoop.rightBol
d_rightBol_1910 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1910 = erased
-- Data.Bool.Properties._.IsRightBolLoop.rightDivides
d_rightDivides_1912 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1912 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Bool.Properties._.IsRightBolLoop.∙-cong
d_'8729''45'cong_1924 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1924 = erased
-- Data.Bool.Properties._.IsRing.*-assoc
d_'42''45'assoc_1934 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1934 = erased
-- Data.Bool.Properties._.IsRing.*-cong
d_'42''45'cong_1936 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1936 = erased
-- Data.Bool.Properties._.IsRing.*-identity
d_'42''45'identity_1942 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1942 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424 (coe v0)
-- Data.Bool.Properties._.IsRing.assoc
d_assoc_1954 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1954 = erased
-- Data.Bool.Properties._.IsRing.comm
d_comm_1956 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1956 = erased
-- Data.Bool.Properties._.IsRing.∙-cong
d_'8729''45'cong_1958 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1958 = erased
-- Data.Bool.Properties._.IsRing.identity
d_identity_1964 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1964 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Bool.Properties._.IsRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1970 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1970 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe v0)
-- Data.Bool.Properties._.IsRing.isGroup
d_isGroup_1978 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1978 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe v0))
-- Data.Bool.Properties._.IsRing.isMagma
d_isMagma_1984 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1984 v0
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
-- Data.Bool.Properties._.IsRing.isMonoid
d_isMonoid_1986 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1986 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Bool.Properties._.IsRing.isSemigroup
d_isSemigroup_1988 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1988 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v0))))
-- Data.Bool.Properties._.IsRing.⁻¹-cong
d_'8315''185''45'cong_1992 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1992 = erased
-- Data.Bool.Properties._.IsRing.inverse
d_inverse_1994 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1994 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v0)))
-- Data.Bool.Properties._.IsRing.distrib
d_distrib_2000 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2000 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2426 (coe v0)
-- Data.Bool.Properties._.IsRing.isEquivalence
d_isEquivalence_2006 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2006 v0
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
-- Data.Bool.Properties._.IsRing.zero
d_zero_2032 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2032 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2428 (coe v0)
-- Data.Bool.Properties._.IsRingWithoutOne.*-assoc
d_'42''45'assoc_2042 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2042 = erased
-- Data.Bool.Properties._.IsRingWithoutOne.*-cong
d_'42''45'cong_2044 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2044 = erased
-- Data.Bool.Properties._.IsRingWithoutOne.assoc
d_assoc_2054 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2054 = erased
-- Data.Bool.Properties._.IsRingWithoutOne.comm
d_comm_2056 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2056 = erased
-- Data.Bool.Properties._.IsRingWithoutOne.∙-cong
d_'8729''45'cong_2058 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2058 = erased
-- Data.Bool.Properties._.IsRingWithoutOne.identity
d_identity_2064 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2064 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Bool.Properties._.IsRingWithoutOne.+-isAbelianGroup
d_'43''45'isAbelianGroup_2070 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2070 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
      (coe v0)
-- Data.Bool.Properties._.IsRingWithoutOne.isGroup
d_isGroup_2078 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_2078 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe v0))
-- Data.Bool.Properties._.IsRingWithoutOne.isMagma
d_isMagma_2084 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2084 v0
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
-- Data.Bool.Properties._.IsRingWithoutOne.isMonoid
d_isMonoid_2086 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2086 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Bool.Properties._.IsRingWithoutOne.isSemigroup
d_isSemigroup_2088 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2088 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe v0))))
-- Data.Bool.Properties._.IsRingWithoutOne.⁻¹-cong
d_'8315''185''45'cong_2092 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_2092 = erased
-- Data.Bool.Properties._.IsRingWithoutOne.inverse
d_inverse_2094 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_2094 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe v0)))
-- Data.Bool.Properties._.IsRingWithoutOne.distrib
d_distrib_2100 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2100 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2082 (coe v0)
-- Data.Bool.Properties._.IsRingWithoutOne.isEquivalence
d_isEquivalence_2106 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2106 v0
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
-- Data.Bool.Properties._.IsRingWithoutOne.zero
d_zero_2124 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2124 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2084 (coe v0)
-- Data.Bool.Properties._.IsSelectiveMagma.isEquivalence
d_isEquivalence_2132 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0))
-- Data.Bool.Properties._.IsSelectiveMagma.isMagma
d_isMagma_2134 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2134 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0)
-- Data.Bool.Properties._.IsSelectiveMagma.sel
d_sel_2142 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  Bool -> Bool -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_2142 v0
  = coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0)
-- Data.Bool.Properties._.IsSelectiveMagma.∙-cong
d_'8729''45'cong_2150 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2150 = erased
-- Data.Bool.Properties._.IsSemigroup.assoc
d_assoc_2158 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2158 = erased
-- Data.Bool.Properties._.IsSemigroup.isEquivalence
d_isEquivalence_2160 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2160 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0))
-- Data.Bool.Properties._.IsSemigroup.isMagma
d_isMagma_2162 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2162 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0)
-- Data.Bool.Properties._.IsSemigroup.∙-cong
d_'8729''45'cong_2176 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2176 = erased
-- Data.Bool.Properties._.IsSemimedialMagma.isEquivalence
d_isEquivalence_2184 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2184 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0))
-- Data.Bool.Properties._.IsSemimedialMagma.isMagma
d_isMagma_2186 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2186 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0)
-- Data.Bool.Properties._.IsSemimedialMagma.semiMedial
d_semiMedial_2194 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semiMedial_2194 v0
  = coe MAlonzo.Code.Algebra.Structures.d_semiMedial_370 (coe v0)
-- Data.Bool.Properties._.IsSemimedialMagma.∙-cong
d_'8729''45'cong_2206 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2206 = erased
-- Data.Bool.Properties._.IsSemiring.*-assoc
d_'42''45'assoc_2214 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2214 = erased
-- Data.Bool.Properties._.IsSemiring.*-cong
d_'42''45'cong_2216 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2216 = erased
-- Data.Bool.Properties._.IsSemiring.*-identity
d_'42''45'identity_2222 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Bool.Properties._.IsSemiring.assoc
d_assoc_2234 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2234 = erased
-- Data.Bool.Properties._.IsSemiring.comm
d_comm_2236 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2236 = erased
-- Data.Bool.Properties._.IsSemiring.∙-cong
d_'8729''45'cong_2238 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2238 = erased
-- Data.Bool.Properties._.IsSemiring.identity
d_identity_2244 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2244 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Bool.Properties._.IsSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2252 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2252 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Bool.Properties._.IsSemiring.isMagma
d_isMagma_2256 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2256 v0
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
-- Data.Bool.Properties._.IsSemiring.isMonoid
d_isMonoid_2258 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2258 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe v0)))
-- Data.Bool.Properties._.IsSemiring.isSemigroup
d_isSemigroup_2260 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2260 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v0))))
-- Data.Bool.Properties._.IsSemiring.distrib
d_distrib_2264 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Bool.Properties._.IsSemiring.isEquivalence
d_isEquivalence_2270 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2270 v0
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
-- Data.Bool.Properties._.IsSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2276 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2276 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe v0)
-- Data.Bool.Properties._.IsSemiring.zero
d_zero_2290 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2290 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1388 (coe v0)
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.*-assoc
d_'42''45'assoc_2298 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2298 = erased
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.*-cong
d_'42''45'cong_2300 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2300 = erased
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.*-identity
d_'42''45'identity_2306 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296 (coe v0)
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.assoc
d_assoc_2318 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2318 = erased
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.comm
d_comm_2320 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2320 = erased
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.∙-cong
d_'8729''45'cong_2322 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2322 = erased
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.identity
d_identity_2328 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2328 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2336 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe v0)
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.isMagma
d_isMagma_2340 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2340 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe v0))))
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.isMonoid
d_isMonoid_2342 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2342 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v0))
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.isSemigroup
d_isSemigroup_2344 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2344 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe v0)))
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.distrib
d_distrib_2348 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2348 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1298 (coe v0)
-- Data.Bool.Properties._.IsSemiringWithoutAnnihilatingZero.isEquivalence
d_isEquivalence_2354 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2354 v0
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
-- Data.Bool.Properties._.IsSemiringWithoutOne.*-assoc
d_'42''45'assoc_2370 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2370 = erased
-- Data.Bool.Properties._.IsSemiringWithoutOne.*-cong
d_'42''45'cong_2372 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2372 = erased
-- Data.Bool.Properties._.IsSemiringWithoutOne.comm
d_comm_2382 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2382 = erased
-- Data.Bool.Properties._.IsSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2386 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2386 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe v0)
-- Data.Bool.Properties._.IsSemiringWithoutOne.isMonoid
d_isMonoid_2390 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2390 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe v0))
-- Data.Bool.Properties._.IsSemiringWithoutOne.distrib
d_distrib_2392 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2392 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1166 (coe v0)
-- Data.Bool.Properties._.IsSemiringWithoutOne.isEquivalence
d_isEquivalence_2394 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2394 v0
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
-- Data.Bool.Properties._.IsSemiringWithoutOne.zero
d_zero_2398 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2398 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1168 (coe v0)
-- Data.Bool.Properties._.IsUnitalMagma.identity
d_identity_2406 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2406 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_568 (coe v0)
-- Data.Bool.Properties._.IsUnitalMagma.isEquivalence
d_isEquivalence_2412 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0))
-- Data.Bool.Properties._.IsUnitalMagma.isMagma
d_isMagma_2414 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2414 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0)
-- Data.Bool.Properties._.IsUnitalMagma.∙-cong
d_'8729''45'cong_2428 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2428 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra
d_IsBooleanAlgebra_2436 a0 a1 a2 a3 a4 = ()
-- Data.Bool.Properties._.IsBoundedJoinSemilattice
d_IsBoundedJoinSemilattice_2438 ::
  (Bool -> Bool -> Bool) -> Bool -> ()
d_IsBoundedJoinSemilattice_2438 = erased
-- Data.Bool.Properties._.IsBoundedMeetSemilattice
d_IsBoundedMeetSemilattice_2440 ::
  (Bool -> Bool -> Bool) -> Bool -> ()
d_IsBoundedMeetSemilattice_2440 = erased
-- Data.Bool.Properties._.IsBoundedSemilattice
d_IsBoundedSemilattice_2442 :: (Bool -> Bool -> Bool) -> Bool -> ()
d_IsBoundedSemilattice_2442 = erased
-- Data.Bool.Properties._.IsDistributiveLattice
d_IsDistributiveLattice_2444 a0 a1 = ()
-- Data.Bool.Properties._.IsJoinSemilattice
d_IsJoinSemilattice_2446 :: (Bool -> Bool -> Bool) -> ()
d_IsJoinSemilattice_2446 = erased
-- Data.Bool.Properties._.IsLattice
d_IsLattice_2448 a0 a1 = ()
-- Data.Bool.Properties._.IsMeetSemilattice
d_IsMeetSemilattice_2450 :: (Bool -> Bool -> Bool) -> ()
d_IsMeetSemilattice_2450 = erased
-- Data.Bool.Properties._.IsSemilattice
d_IsSemilattice_2452 a0 = ()
-- Data.Bool.Properties._.IsBooleanAlgebra.absorptive
d_absorptive_2456 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_2456 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe v0)))
-- Data.Bool.Properties._.IsBooleanAlgebra.isDistributiveLattice
d_isDistributiveLattice_2458 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_isDistributiveLattice_2458 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
      (coe v0)
-- Data.Bool.Properties._.IsBooleanAlgebra.isEquivalence
d_isEquivalence_2460 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2460 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe v0)))
-- Data.Bool.Properties._.IsBooleanAlgebra.isLattice
d_isLattice_2462 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_2462 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe v0))
-- Data.Bool.Properties._.IsBooleanAlgebra.¬-cong
d_'172''45'cong_2474 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'172''45'cong_2474 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra.∧-assoc
d_'8743''45'assoc_2478 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'assoc_2478 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra.∧-comm
d_'8743''45'comm_2480 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'comm_2480 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra.∧-complement
d_'8743''45'complement_2482 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'complement_2482 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'complement_2918
      (coe v0)
-- Data.Bool.Properties._.IsBooleanAlgebra.∧-cong
d_'8743''45'cong_2488 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'cong_2488 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra.∧-distrib-∨
d_'8743''45'distrib'45''8744'_2494 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_2494 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'distrib'45''8744'_2834
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe v0))
-- Data.Bool.Properties._.IsBooleanAlgebra.∨-assoc
d_'8744''45'assoc_2502 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'assoc_2502 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra.∨-comm
d_'8744''45'comm_2504 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'comm_2504 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra.∨-complement
d_'8744''45'complement_2506 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'complement_2506 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'complement_2916
      (coe v0)
-- Data.Bool.Properties._.IsBooleanAlgebra.∨-cong
d_'8744''45'cong_2512 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'cong_2512 = erased
-- Data.Bool.Properties._.IsBooleanAlgebra.∨-distrib-∧
d_'8744''45'distrib'45''8743'_2518 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_2518 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'distrib'45''8743'_2832
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe v0))
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.assoc
d_assoc_2526 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2526 = erased
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.comm
d_comm_2528 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2528 = erased
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.idem
d_idem_2530 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_2530 = erased
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.identity
d_identity_2532 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2532 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.isBand
d_isBand_2538 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2538 ~v0 ~v1 v2 = du_isBand_2538 v2
du_isBand_2538 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_2538 v0
  = coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v0)
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.isEquivalence
d_isEquivalence_2540 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2540 ~v0 ~v1 v2 = du_isEquivalence_2540 v2
du_isEquivalence_2540 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_2540 v0
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
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.isSemilattice
d_isSemilattice_2542 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isSemilattice_2542 ~v0 ~v1 v2 = du_isSemilattice_2542 v2
du_isSemilattice_2542 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_isSemilattice_2542 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
      (coe v0)
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.isMagma
d_isMagma_2544 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2544 ~v0 ~v1 v2 = du_isMagma_2544 v2
du_isMagma_2544 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_2544 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.isSemigroup
d_isSemigroup_2548 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2548 ~v0 ~v1 v2 = du_isSemigroup_2548 v2
du_isSemigroup_2548 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_2548 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsBoundedJoinSemilattice.∙-cong
d_'8729''45'cong_2560 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2560 = erased
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.assoc
d_assoc_2568 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2568 = erased
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.comm
d_comm_2570 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2570 = erased
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.idem
d_idem_2572 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_2572 = erased
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.identity
d_identity_2574 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.isBand
d_isBand_2580 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2580 ~v0 ~v1 v2 = du_isBand_2580 v2
du_isBand_2580 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_2580 v0
  = coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v0)
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.isEquivalence
d_isEquivalence_2582 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2582 ~v0 ~v1 v2 = du_isEquivalence_2582 v2
du_isEquivalence_2582 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_2582 v0
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
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.isMagma
d_isMagma_2584 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2584 ~v0 ~v1 v2 = du_isMagma_2584 v2
du_isMagma_2584 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_2584 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.isSemilattice
d_isSemilattice_2586 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isSemilattice_2586 ~v0 ~v1 v2 = du_isSemilattice_2586 v2
du_isSemilattice_2586 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_isSemilattice_2586 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
      (coe v0)
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.isSemigroup
d_isSemigroup_2590 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2590 ~v0 ~v1 v2 = du_isSemigroup_2590 v2
du_isSemigroup_2590 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_2590 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsBoundedMeetSemilattice.∙-cong
d_'8729''45'cong_2602 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2602 = erased
-- Data.Bool.Properties._.IsBoundedSemilattice.assoc
d_assoc_2610 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2610 = erased
-- Data.Bool.Properties._.IsBoundedSemilattice.comm
d_comm_2612 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2612 = erased
-- Data.Bool.Properties._.IsBoundedSemilattice.idem
d_idem_2614 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_2614 = erased
-- Data.Bool.Properties._.IsBoundedSemilattice.identity
d_identity_2616 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2616 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsBoundedSemilattice.isCommutativeMonoid
d_isCommutativeMonoid_2626 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2626 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0)
-- Data.Bool.Properties._.IsBoundedSemilattice.isEquivalence
d_isEquivalence_2630 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2630 v0
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
-- Data.Bool.Properties._.IsBoundedSemilattice.isMagma
d_isMagma_2632 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2632 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Data.Bool.Properties._.IsBoundedSemilattice.isMonoid
d_isMonoid_2634 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2634 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Data.Bool.Properties._.IsBoundedSemilattice.isSemigroup
d_isSemigroup_2638 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2638 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Data.Bool.Properties._.IsBoundedSemilattice.isSemilattice
d_isSemilattice_2640 ::
  (Bool -> Bool -> Bool) ->
  Bool ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isSemilattice_2640 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630 v2
-- Data.Bool.Properties._.IsBoundedSemilattice.∙-cong
d_'8729''45'cong_2654 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2654 = erased
-- Data.Bool.Properties._.IsDistributiveLattice.absorptive
d_absorptive_2662 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_2662 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Data.Bool.Properties._.IsDistributiveLattice.isEquivalence
d_isEquivalence_2664 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2664 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Data.Bool.Properties._.IsDistributiveLattice.isLattice
d_isLattice_2666 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_2666 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0)
-- Data.Bool.Properties._.IsDistributiveLattice.∧-assoc
d_'8743''45'assoc_2680 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'assoc_2680 = erased
-- Data.Bool.Properties._.IsDistributiveLattice.∧-comm
d_'8743''45'comm_2682 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'comm_2682 = erased
-- Data.Bool.Properties._.IsDistributiveLattice.∧-cong
d_'8743''45'cong_2684 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'cong_2684 = erased
-- Data.Bool.Properties._.IsDistributiveLattice.∧-distrib-∨
d_'8743''45'distrib'45''8744'_2690 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_2690 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'distrib'45''8744'_2834
      (coe v0)
-- Data.Bool.Properties._.IsDistributiveLattice.∨-assoc
d_'8744''45'assoc_2698 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'assoc_2698 = erased
-- Data.Bool.Properties._.IsDistributiveLattice.∨-comm
d_'8744''45'comm_2700 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'comm_2700 = erased
-- Data.Bool.Properties._.IsDistributiveLattice.∨-cong
d_'8744''45'cong_2702 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'cong_2702 = erased
-- Data.Bool.Properties._.IsDistributiveLattice.∨-distrib-∧
d_'8744''45'distrib'45''8743'_2708 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_2708 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'distrib'45''8743'_2832
      (coe v0)
-- Data.Bool.Properties._.IsJoinSemilattice.assoc
d_assoc_2716 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2716 = erased
-- Data.Bool.Properties._.IsJoinSemilattice.comm
d_comm_2718 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2718 = erased
-- Data.Bool.Properties._.IsJoinSemilattice.idem
d_idem_2720 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_2720 = erased
-- Data.Bool.Properties._.IsJoinSemilattice.isBand
d_isBand_2722 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2722 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0)
-- Data.Bool.Properties._.IsJoinSemilattice.isEquivalence
d_isEquivalence_2724 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2724 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0))))
-- Data.Bool.Properties._.IsJoinSemilattice.isMagma
d_isMagma_2726 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2726 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0)))
-- Data.Bool.Properties._.IsJoinSemilattice.isSemigroup
d_isSemigroup_2730 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2730 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0))
-- Data.Bool.Properties._.IsJoinSemilattice.∙-cong
d_'8729''45'cong_2742 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2742 = erased
-- Data.Bool.Properties._.IsLattice.absorptive
d_absorptive_2750 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_2750 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780 (coe v0)
-- Data.Bool.Properties._.IsLattice.isEquivalence
d_isEquivalence_2752 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2752 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe v0)
-- Data.Bool.Properties._.IsLattice.∧-assoc
d_'8743''45'assoc_2766 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'assoc_2766 = erased
-- Data.Bool.Properties._.IsLattice.∧-comm
d_'8743''45'comm_2768 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'comm_2768 = erased
-- Data.Bool.Properties._.IsLattice.∧-cong
d_'8743''45'cong_2770 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'cong_2770 = erased
-- Data.Bool.Properties._.IsLattice.∨-assoc
d_'8744''45'assoc_2778 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'assoc_2778 = erased
-- Data.Bool.Properties._.IsLattice.∨-comm
d_'8744''45'comm_2780 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'comm_2780 = erased
-- Data.Bool.Properties._.IsLattice.∨-cong
d_'8744''45'cong_2782 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'cong_2782 = erased
-- Data.Bool.Properties._.IsMeetSemilattice.assoc
d_assoc_2790 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2790 = erased
-- Data.Bool.Properties._.IsMeetSemilattice.comm
d_comm_2792 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2792 = erased
-- Data.Bool.Properties._.IsMeetSemilattice.idem
d_idem_2794 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_2794 = erased
-- Data.Bool.Properties._.IsMeetSemilattice.isBand
d_isBand_2796 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2796 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0)
-- Data.Bool.Properties._.IsMeetSemilattice.isEquivalence
d_isEquivalence_2798 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2798 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0))))
-- Data.Bool.Properties._.IsMeetSemilattice.isMagma
d_isMagma_2800 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2800 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0)))
-- Data.Bool.Properties._.IsMeetSemilattice.isSemigroup
d_isSemigroup_2804 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2804 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0))
-- Data.Bool.Properties._.IsMeetSemilattice.∙-cong
d_'8729''45'cong_2816 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2816 = erased
-- Data.Bool.Properties._.IsSemilattice.assoc
d_assoc_2824 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2824 = erased
-- Data.Bool.Properties._.IsSemilattice.comm
d_comm_2826 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2826 = erased
-- Data.Bool.Properties._.IsSemilattice.idem
d_idem_2828 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_2828 = erased
-- Data.Bool.Properties._.IsSemilattice.isBand
d_isBand_2830 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2830 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0)
-- Data.Bool.Properties._.IsSemilattice.isEquivalence
d_isEquivalence_2832 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2832 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0))))
-- Data.Bool.Properties._.IsSemilattice.isMagma
d_isMagma_2834 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2834 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0)))
-- Data.Bool.Properties._.IsSemilattice.isSemigroup
d_isSemigroup_2838 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2838 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0))
-- Data.Bool.Properties._.IsSemilattice.∙-cong
d_'8729''45'cong_2850 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2850 = erased
-- Data.Bool.Properties._≟_
d__'8799'__2864 ::
  Bool ->
  Bool -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__2864 v0 v1
  = if coe v0
      then if coe v1
             then coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe v1)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             else coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe v1) (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      else (if coe v1
              then coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                     (coe v0) (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
              else coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                     (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                     (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased))
-- Data.Bool.Properties.≡-setoid
d_'8801''45'setoid_2866 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''45'setoid_2866
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402
-- Data.Bool.Properties.≡-decSetoid
d_'8801''45'decSetoid_2868 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8801''45'decSetoid_2868
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
      (coe d__'8799'__2864)
-- Data.Bool.Properties.≤-reflexive
d_'8804''45'reflexive_2870 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10
d_'8804''45'reflexive_2870 ~v0 ~v1 ~v2
  = du_'8804''45'reflexive_2870
du_'8804''45'reflexive_2870 ::
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10
du_'8804''45'reflexive_2870
  = coe MAlonzo.Code.Data.Bool.Base.C_b'8804'b_16
-- Data.Bool.Properties.≤-refl
d_'8804''45'refl_2872 ::
  Bool -> MAlonzo.Code.Data.Bool.Base.T__'8804'__10
d_'8804''45'refl_2872 ~v0 = du_'8804''45'refl_2872
du_'8804''45'refl_2872 :: MAlonzo.Code.Data.Bool.Base.T__'8804'__10
du_'8804''45'refl_2872 = coe du_'8804''45'reflexive_2870
-- Data.Bool.Properties.≤-trans
d_'8804''45'trans_2874 ::
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10
d_'8804''45'trans_2874 ~v0 ~v1 ~v2 v3 v4
  = du_'8804''45'trans_2874 v3 v4
du_'8804''45'trans_2874 ::
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10
du_'8804''45'trans_2874 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Bool.Base.C_f'8804't_12
        -> coe seq (coe v1) (coe v0)
      MAlonzo.Code.Data.Bool.Base.C_b'8804'b_16 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Bool.Properties.≤-antisym
d_'8804''45'antisym_2878 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'antisym_2878 = erased
-- Data.Bool.Properties.≤-minimum
d_'8804''45'minimum_2880 ::
  Bool -> MAlonzo.Code.Data.Bool.Base.T__'8804'__10
d_'8804''45'minimum_2880 v0
  = if coe v0
      then coe MAlonzo.Code.Data.Bool.Base.C_f'8804't_12
      else coe MAlonzo.Code.Data.Bool.Base.C_b'8804'b_16
-- Data.Bool.Properties.≤-maximum
d_'8804''45'maximum_2882 ::
  Bool -> MAlonzo.Code.Data.Bool.Base.T__'8804'__10
d_'8804''45'maximum_2882 v0
  = if coe v0
      then coe MAlonzo.Code.Data.Bool.Base.C_b'8804'b_16
      else coe MAlonzo.Code.Data.Bool.Base.C_f'8804't_12
-- Data.Bool.Properties.≤-total
d_'8804''45'total_2884 ::
  Bool -> Bool -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8804''45'total_2884 v0 v1
  = if coe v0
      then coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
             (coe d_'8804''45'maximum_2882 (coe v1))
      else coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
             (coe d_'8804''45'minimum_2880 (coe v1))
-- Data.Bool.Properties._≤?_
d__'8804''63'__2890 ::
  Bool ->
  Bool -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__2890 v0 v1
  = if coe v0
      then if coe v1
             then coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe v1)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                       (coe MAlonzo.Code.Data.Bool.Base.C_b'8804'b_16))
             else coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe v1) (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      else coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe d_'8804''45'minimum_2880 (coe v1)))
-- Data.Bool.Properties.≤-irrelevant
d_'8804''45'irrelevant_2894 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'irrelevant_2894 = erased
-- Data.Bool.Properties.≤-isPreorder
d_'8804''45'isPreorder_2896 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder_2896
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'8804''45'reflexive_2870)
      (\ v0 v1 v2 v3 v4 -> coe du_'8804''45'trans_2874 v3 v4)
-- Data.Bool.Properties.≤-isPartialOrder
d_'8804''45'isPartialOrder_2898 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8804''45'isPartialOrder_2898
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe d_'8804''45'isPreorder_2896) erased
-- Data.Bool.Properties.≤-isTotalOrder
d_'8804''45'isTotalOrder_2900 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8804''45'isTotalOrder_2900
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe d_'8804''45'isPartialOrder_2898) (coe d_'8804''45'total_2884)
-- Data.Bool.Properties.≤-isDecTotalOrder
d_'8804''45'isDecTotalOrder_2902 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_'8804''45'isDecTotalOrder_2902
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe d_'8804''45'isTotalOrder_2900) (coe d__'8799'__2864)
      (coe d__'8804''63'__2890)
-- Data.Bool.Properties.≤-poset
d_'8804''45'poset_2904 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8804''45'poset_2904
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      d_'8804''45'isPartialOrder_2898
-- Data.Bool.Properties.≤-preorder
d_'8804''45'preorder_2906 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8804''45'preorder_2906
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      d_'8804''45'isPreorder_2896
-- Data.Bool.Properties.≤-totalOrder
d_'8804''45'totalOrder_2908 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_'8804''45'totalOrder_2908
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      d_'8804''45'isTotalOrder_2900
-- Data.Bool.Properties.≤-decTotalOrder
d_'8804''45'decTotalOrder_2910 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_'8804''45'decTotalOrder_2910
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      d_'8804''45'isDecTotalOrder_2902
-- Data.Bool.Properties.<-irrefl
d_'60''45'irrefl_2912 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl_2912 = erased
-- Data.Bool.Properties.<-asym
d_'60''45'asym_2914 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'asym_2914 = erased
-- Data.Bool.Properties.<-trans
d_'60''45'trans_2916 ::
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18
d_'60''45'trans_2916 = erased
-- Data.Bool.Properties.<-transʳ
d_'60''45'trans'691'_2918 ::
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18
d_'60''45'trans'691'_2918 = erased
-- Data.Bool.Properties.<-transˡ
d_'60''45'trans'737'_2920 ::
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Bool.Base.T__'8804'__10 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18
d_'60''45'trans'737'_2920 = erased
-- Data.Bool.Properties.<-cmp
d_'60''45'cmp_2922 ::
  Bool -> Bool -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_'60''45'cmp_2922 v0 v1
  = if coe v0
      then if coe v1
             then coe
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased
             else coe
                    MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 erased
      else (if coe v1
              then coe
                     MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 erased
              else coe
                     MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased)
-- Data.Bool.Properties._<?_
d__'60''63'__2924 ::
  Bool ->
  Bool -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__2924 v0 v1
  = if coe v0
      then coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      else (if coe v1
              then coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                     (coe v1)
                     (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
              else coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                     (coe v1)
                     (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
-- Data.Bool.Properties.<-resp₂-≡
d_'60''45'resp'8322''45''8801'_2926 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'8322''45''8801'_2926
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4))
-- Data.Bool.Properties.<-irrelevant
d_'60''45'irrelevant_2932 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''45'irrelevant_2932 = erased
-- Data.Bool.Properties.<-wellFounded
d_'60''45'wellFounded_2934 ::
  Bool -> MAlonzo.Code.Induction.WellFounded.T_Acc_42
d_'60''45'wellFounded_2934 = erased
-- Data.Bool.Properties._.<-acc
d_'60''45'acc_2944 ::
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Data.Bool.Base.T__'60'__18 ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42
d_'60''45'acc_2944 = erased
-- Data.Bool.Properties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_2948 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_2948
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased d_'60''45'resp'8322''45''8801'_2926
-- Data.Bool.Properties.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_2950 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_2950
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased (coe d_'60''45'cmp_2922)
-- Data.Bool.Properties.<-strictPartialOrder
d_'60''45'strictPartialOrder_2952 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_2952
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      d_'60''45'isStrictPartialOrder_2948
-- Data.Bool.Properties.<-strictTotalOrder
d_'60''45'strictTotalOrder_2954 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_2954
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      d_'60''45'isStrictTotalOrder_2950
-- Data.Bool.Properties.∨-assoc
d_'8744''45'assoc_2956 ::
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'assoc_2956 = erased
-- Data.Bool.Properties.∨-comm
d_'8744''45'comm_2966 ::
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'comm_2966 = erased
-- Data.Bool.Properties.∨-identityˡ
d_'8744''45'identity'737'_2968 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'identity'737'_2968 = erased
-- Data.Bool.Properties.∨-identityʳ
d_'8744''45'identity'691'_2970 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'identity'691'_2970 = erased
-- Data.Bool.Properties.∨-identity
d_'8744''45'identity_2972 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'identity_2972
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∨-zeroˡ
d_'8744''45'zero'737'_2974 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'zero'737'_2974 = erased
-- Data.Bool.Properties.∨-zeroʳ
d_'8744''45'zero'691'_2976 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'zero'691'_2976 = erased
-- Data.Bool.Properties.∨-zero
d_'8744''45'zero_2978 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'zero_2978
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∨-inverseˡ
d_'8744''45'inverse'737'_2980 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'inverse'737'_2980 = erased
-- Data.Bool.Properties.∨-inverseʳ
d_'8744''45'inverse'691'_2982 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'inverse'691'_2982 = erased
-- Data.Bool.Properties.∨-inverse
d_'8744''45'inverse_2986 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'inverse_2986
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∨-idem
d_'8744''45'idem_2988 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'idem_2988 = erased
-- Data.Bool.Properties.∨-sel
d_'8744''45'sel_2990 ::
  Bool -> Bool -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8744''45'sel_2990 v0 ~v1 = du_'8744''45'sel_2990 v0
du_'8744''45'sel_2990 ::
  Bool -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8744''45'sel_2990 v0
  = if coe v0
      then coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
      else coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
-- Data.Bool.Properties.∨-isMagma
d_'8744''45'isMagma_2996 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8744''45'isMagma_2996
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Bool.Properties.∨-magma
d_'8744''45'magma_2998 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8744''45'magma_2998
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30 d_'8744''45'isMagma_2996
-- Data.Bool.Properties.∨-isSemigroup
d_'8744''45'isSemigroup_3000 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8744''45'isSemigroup_3000
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'8744''45'isMagma_2996) erased
-- Data.Bool.Properties.∨-semigroup
d_'8744''45'semigroup_3002 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8744''45'semigroup_3002
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      d_'8744''45'isSemigroup_3000
-- Data.Bool.Properties.∨-isBand
d_'8744''45'isBand_3004 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8744''45'isBand_3004
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsBand'46'constructor_10089
      (coe d_'8744''45'isSemigroup_3000) erased
-- Data.Bool.Properties.∨-band
d_'8744''45'band_3006 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8744''45'band_3006
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Band'46'constructor_9627
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30 d_'8744''45'isBand_3004
-- Data.Bool.Properties.∨-isSemilattice
d_'8744''45'isSemilattice_3008 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8744''45'isSemilattice_3008
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsSemilattice'46'constructor_31019
      (coe d_'8744''45'isBand_3004) erased
-- Data.Bool.Properties.∨-semilattice
d_'8744''45'semilattice_3010 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8744''45'semilattice_3010
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_Semilattice'46'constructor_187
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      d_'8744''45'isSemilattice_3008
-- Data.Bool.Properties.∨-isMonoid
d_'8744''45'isMonoid_3012 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8744''45'isMonoid_3012
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'8744''45'isSemigroup_3000) (coe d_'8744''45'identity_2972)
-- Data.Bool.Properties.∨-isCommutativeMonoid
d_'8744''45'isCommutativeMonoid_3014 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'8744''45'isCommutativeMonoid_3014
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'8744''45'isMonoid_3012) erased
-- Data.Bool.Properties.∨-commutativeMonoid
d_'8744''45'commutativeMonoid_3016 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'8744''45'commutativeMonoid_3016
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
      d_'8744''45'isCommutativeMonoid_3014
-- Data.Bool.Properties.∨-isIdempotentCommutativeMonoid
d_'8744''45'isIdempotentCommutativeMonoid_3018 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
d_'8744''45'isIdempotentCommutativeMonoid_3018
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsIdempotentCommutativeMonoid'46'constructor_16919
      (coe d_'8744''45'isCommutativeMonoid_3014) erased
-- Data.Bool.Properties.∨-idempotentCommutativeMonoid
d_'8744''45'idempotentCommutativeMonoid_3020 ::
  MAlonzo.Code.Algebra.Bundles.T_IdempotentCommutativeMonoid_916
d_'8744''45'idempotentCommutativeMonoid_3020
  = coe
      MAlonzo.Code.Algebra.Bundles.C_IdempotentCommutativeMonoid'46'constructor_16849
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
      d_'8744''45'isIdempotentCommutativeMonoid_3018
-- Data.Bool.Properties.∧-assoc
d_'8743''45'assoc_3022 ::
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'assoc_3022 = erased
-- Data.Bool.Properties.∧-comm
d_'8743''45'comm_3032 ::
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'comm_3032 = erased
-- Data.Bool.Properties.∧-identityˡ
d_'8743''45'identity'737'_3034 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'identity'737'_3034 = erased
-- Data.Bool.Properties.∧-identityʳ
d_'8743''45'identity'691'_3036 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'identity'691'_3036 = erased
-- Data.Bool.Properties.∧-identity
d_'8743''45'identity_3038 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'identity_3038
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∧-zeroˡ
d_'8743''45'zero'737'_3040 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'zero'737'_3040 = erased
-- Data.Bool.Properties.∧-zeroʳ
d_'8743''45'zero'691'_3042 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'zero'691'_3042 = erased
-- Data.Bool.Properties.∧-zero
d_'8743''45'zero_3044 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'zero_3044
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∧-inverseˡ
d_'8743''45'inverse'737'_3046 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'inverse'737'_3046 = erased
-- Data.Bool.Properties.∧-inverseʳ
d_'8743''45'inverse'691'_3048 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'inverse'691'_3048 = erased
-- Data.Bool.Properties.∧-inverse
d_'8743''45'inverse_3052 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'inverse_3052
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∧-idem
d_'8743''45'idem_3054 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'idem_3054 = erased
-- Data.Bool.Properties.∧-sel
d_'8743''45'sel_3056 ::
  Bool -> Bool -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8743''45'sel_3056 v0 ~v1 = du_'8743''45'sel_3056 v0
du_'8743''45'sel_3056 ::
  Bool -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8743''45'sel_3056 v0
  = if coe v0
      then coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
      else coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
-- Data.Bool.Properties.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_3062 ::
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'distrib'737''45''8744'_3062 = erased
-- Data.Bool.Properties.∧-distribʳ-∨
d_'8743''45'distrib'691''45''8744'_3072 ::
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'distrib'691''45''8744'_3072 = erased
-- Data.Bool.Properties.∧-distrib-∨
d_'8743''45'distrib'45''8744'_3080 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_3080
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∨-distribˡ-∧
d_'8744''45'distrib'737''45''8743'_3082 ::
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'distrib'737''45''8743'_3082 = erased
-- Data.Bool.Properties.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_3092 ::
  Bool ->
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'distrib'691''45''8743'_3092 = erased
-- Data.Bool.Properties.∨-distrib-∧
d_'8744''45'distrib'45''8743'_3100 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_3100
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∧-abs-∨
d_'8743''45'abs'45''8744'_3102 ::
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8743''45'abs'45''8744'_3102 = erased
-- Data.Bool.Properties.∨-abs-∧
d_'8744''45'abs'45''8743'_3108 ::
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8744''45'abs'45''8743'_3108 = erased
-- Data.Bool.Properties.∨-∧-absorptive
d_'8744''45''8743''45'absorptive_3114 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45''8743''45'absorptive_3114
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Bool.Properties.∧-isMagma
d_'8743''45'isMagma_3116 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8743''45'isMagma_3116
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased
-- Data.Bool.Properties.∧-magma
d_'8743''45'magma_3118 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8743''45'magma_3118
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24 d_'8743''45'isMagma_3116
-- Data.Bool.Properties.∧-isSemigroup
d_'8743''45'isSemigroup_3120 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8743''45'isSemigroup_3120
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe d_'8743''45'isMagma_3116) erased
-- Data.Bool.Properties.∧-semigroup
d_'8743''45'semigroup_3122 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8743''45'semigroup_3122
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      d_'8743''45'isSemigroup_3120
-- Data.Bool.Properties.∧-isBand
d_'8743''45'isBand_3124 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8743''45'isBand_3124
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsBand'46'constructor_10089
      (coe d_'8743''45'isSemigroup_3120) erased
-- Data.Bool.Properties.∧-band
d_'8743''45'band_3126 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8743''45'band_3126
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Band'46'constructor_9627
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24 d_'8743''45'isBand_3124
-- Data.Bool.Properties.∧-isSemilattice
d_'8743''45'isSemilattice_3128 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8743''45'isSemilattice_3128
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsSemilattice'46'constructor_31019
      (coe d_'8743''45'isBand_3124) erased
-- Data.Bool.Properties.∧-semilattice
d_'8743''45'semilattice_3130 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8743''45'semilattice_3130
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_Semilattice'46'constructor_187
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      d_'8743''45'isSemilattice_3128
-- Data.Bool.Properties.∧-isMonoid
d_'8743''45'isMonoid_3132 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8743''45'isMonoid_3132
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe d_'8743''45'isSemigroup_3120) (coe d_'8743''45'identity_3038)
-- Data.Bool.Properties.∧-isCommutativeMonoid
d_'8743''45'isCommutativeMonoid_3134 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'8743''45'isCommutativeMonoid_3134
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe d_'8743''45'isMonoid_3132) erased
-- Data.Bool.Properties.∧-commutativeMonoid
d_'8743''45'commutativeMonoid_3136 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'8743''45'commutativeMonoid_3136
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      d_'8743''45'isCommutativeMonoid_3134
-- Data.Bool.Properties.∧-isIdempotentCommutativeMonoid
d_'8743''45'isIdempotentCommutativeMonoid_3138 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
d_'8743''45'isIdempotentCommutativeMonoid_3138
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsIdempotentCommutativeMonoid'46'constructor_16919
      (coe d_'8743''45'isCommutativeMonoid_3134) erased
-- Data.Bool.Properties.∧-idempotentCommutativeMonoid
d_'8743''45'idempotentCommutativeMonoid_3140 ::
  MAlonzo.Code.Algebra.Bundles.T_IdempotentCommutativeMonoid_916
d_'8743''45'idempotentCommutativeMonoid_3140
  = coe
      MAlonzo.Code.Algebra.Bundles.C_IdempotentCommutativeMonoid'46'constructor_16849
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      d_'8743''45'isIdempotentCommutativeMonoid_3138
-- Data.Bool.Properties.∨-∧-isSemiring
d_'8744''45''8743''45'isSemiring_3142 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'8744''45''8743''45'isSemiring_3142
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
         (coe d_'8744''45'isCommutativeMonoid_3014) erased erased
         (coe d_'8743''45'identity_3038)
         (coe d_'8743''45'distrib'45''8744'_3080))
      (coe d_'8743''45'zero_3044)
-- Data.Bool.Properties.∨-∧-isCommutativeSemiring
d_'8744''45''8743''45'isCommutativeSemiring_3144 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'8744''45''8743''45'isCommutativeSemiring_3144
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe d_'8744''45''8743''45'isSemiring_3142) erased
-- Data.Bool.Properties.∨-∧-commutativeSemiring
d_'8744''45''8743''45'commutativeSemiring_3146 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'8744''45''8743''45'commutativeSemiring_3146
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      d_'8744''45''8743''45'isCommutativeSemiring_3144
-- Data.Bool.Properties.∧-∨-isSemiring
d_'8743''45''8744''45'isSemiring_3148 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'8743''45''8744''45'isSemiring_3148
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
         (coe d_'8743''45'isCommutativeMonoid_3134) erased erased
         (coe d_'8744''45'identity_2972)
         (coe d_'8744''45'distrib'45''8743'_3100))
      (coe d_'8744''45'zero_2978)
-- Data.Bool.Properties.∧-∨-isCommutativeSemiring
d_'8743''45''8744''45'isCommutativeSemiring_3150 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'8743''45''8744''45'isCommutativeSemiring_3150
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe d_'8743''45''8744''45'isSemiring_3148) erased
-- Data.Bool.Properties.∧-∨-commutativeSemiring
d_'8743''45''8744''45'commutativeSemiring_3152 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'8743''45''8744''45'commutativeSemiring_3152
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
      d_'8743''45''8744''45'isCommutativeSemiring_3150
-- Data.Bool.Properties.∨-∧-isLattice
d_'8744''45''8743''45'isLattice_3154 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8744''45''8743''45'isLattice_3154
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsLattice'46'constructor_34033
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      erased erased erased erased erased erased
      (coe d_'8744''45''8743''45'absorptive_3114)
-- Data.Bool.Properties.∨-∧-lattice
d_'8744''45''8743''45'lattice_3156 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8744''45''8743''45'lattice_3156
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      d_'8744''45''8743''45'isLattice_3154
-- Data.Bool.Properties.∨-∧-isDistributiveLattice
d_'8744''45''8743''45'isDistributiveLattice_3158 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8744''45''8743''45'isDistributiveLattice_3158
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsDistributiveLattice'46'constructor_38127
      (coe d_'8744''45''8743''45'isLattice_3154)
      (coe d_'8744''45'distrib'45''8743'_3100)
      (coe d_'8743''45'distrib'45''8744'_3080)
-- Data.Bool.Properties.∨-∧-distributiveLattice
d_'8744''45''8743''45'distributiveLattice_3160 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8744''45''8743''45'distributiveLattice_3160
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_DistributiveLattice'46'constructor_9473
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      d_'8744''45''8743''45'isDistributiveLattice_3158
-- Data.Bool.Properties.∨-∧-isBooleanAlgebra
d_'8744''45''8743''45'isBooleanAlgebra_3162 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894
d_'8744''45''8743''45'isBooleanAlgebra_3162
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsBooleanAlgebra'46'constructor_41193
      (coe d_'8744''45''8743''45'isDistributiveLattice_3158)
      (coe d_'8744''45'inverse_2986) (coe d_'8743''45'inverse_3052)
      erased
-- Data.Bool.Properties.∨-∧-booleanAlgebra
d_'8744''45''8743''45'booleanAlgebra_3164 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680
d_'8744''45''8743''45'booleanAlgebra_3164
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_BooleanAlgebra'46'constructor_11433
      MAlonzo.Code.Data.Bool.Base.d__'8744'__30
      MAlonzo.Code.Data.Bool.Base.d__'8743'__24
      MAlonzo.Code.Data.Bool.Base.d_not_22
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
      d_'8744''45''8743''45'isBooleanAlgebra_3162
-- Data.Bool.Properties.xor-is-ok
d_xor'45'is'45'ok_3170 ::
  Bool -> Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_xor'45'is'45'ok_3170 = erased
-- Data.Bool.Properties.xor-∧-commutativeRing
d_xor'45''8743''45'commutativeRing_3176 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
d_xor'45''8743''45'commutativeRing_3176
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.BooleanAlgebra.du_'8853''45''8743''45'commutativeRing_3402
      (coe d_'8744''45''8743''45'booleanAlgebra_3164)
      (coe MAlonzo.Code.Data.Bool.Base.d__xor__36) erased
-- Data.Bool.Properties.not-involutive
d_not'45'involutive_3442 ::
  Bool -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_not'45'involutive_3442 = erased
-- Data.Bool.Properties.not-injective
d_not'45'injective_3448 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_not'45'injective_3448 = erased
-- Data.Bool.Properties.not-¬
d_not'45''172'_3458 ::
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_not'45''172'_3458 = erased
-- Data.Bool.Properties.¬-not
d_'172''45'not_3464 ::
  Bool ->
  Bool ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'172''45'not_3464 = erased
-- Data.Bool.Properties.⇔→≡
d_'8660''8594''8801'_3476 ::
  Bool ->
  Bool ->
  Bool ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8660''8594''8801'_3476 = erased
-- Data.Bool.Properties.T-≡
d_T'45''8801'_3492 ::
  Bool -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_T'45''8801'_3492 v0
  = if coe v0
      then coe
             MAlonzo.Code.Function.Equivalence.du_equivalence_56 erased
             (let v1 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8 in
              coe (\ v2 -> v1))
      else coe
             MAlonzo.Code.Function.Equivalence.du_equivalence_56 erased erased
-- Data.Bool.Properties.T-not-≡
d_T'45'not'45''8801'_3496 ::
  Bool -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_T'45'not'45''8801'_3496 v0
  = if coe v0
      then coe
             MAlonzo.Code.Function.Equivalence.du_equivalence_56 erased erased
      else coe
             MAlonzo.Code.Function.Equivalence.du_equivalence_56 erased
             (let v1 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8 in
              coe (\ v2 -> v1))
-- Data.Bool.Properties.T-∧
d_T'45''8743'_3502 ::
  Bool -> Bool -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_T'45''8743'_3502 v0 v1
  = if coe v0
      then if coe v1
             then coe
                    MAlonzo.Code.Function.Equivalence.du_equivalence_56
                    (let v2
                           = coe
                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                               (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
                               (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8) in
                     coe (\ v3 -> v2))
                    (let v2 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8 in
                     coe (\ v3 -> v2))
             else coe
                    MAlonzo.Code.Function.Equivalence.du_equivalence_56 erased
                    (coe (\ v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v2)))
      else coe
             MAlonzo.Code.Function.Equivalence.du_equivalence_56 erased
             (coe (\ v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2)))
-- Data.Bool.Properties.T-∨
d_T'45''8744'_3508 ::
  Bool -> Bool -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_T'45''8744'_3508 v0 v1
  = if coe v0
      then coe
             MAlonzo.Code.Function.Equivalence.du_equivalence_56
             (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
             (let v2 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8 in
              coe (\ v3 -> v2))
      else (if coe v1
              then coe
                     MAlonzo.Code.Function.Equivalence.du_equivalence_56
                     (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)
                     (let v2 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8 in
                      coe (\ v3 -> v2))
              else coe
                     MAlonzo.Code.Function.Equivalence.du_equivalence_56
                     (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
                     (coe
                        MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52 (coe (\ v2 -> v2))
                        (coe (\ v2 -> v2))))
-- Data.Bool.Properties.T-irrelevant
d_T'45'irrelevant_3510 ::
  Bool ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_T'45'irrelevant_3510 = erased
-- Data.Bool.Properties.T?
d_T'63'_3512 ::
  Bool -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_T'63'_3512 v0
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
      (coe v0)
      (if coe v0
         then coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
         else coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
-- Data.Bool.Properties.T?-diag
d_T'63''45'diag_3518 :: Bool -> AgdaAny -> AgdaAny
d_T'63''45'diag_3518 v0 ~v1 = du_T'63''45'diag_3518 v0
du_T'63''45'diag_3518 :: Bool -> AgdaAny
du_T'63''45'diag_3518 v0
  = coe seq (coe v0) (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Bool.Properties.push-function-into-if
d_push'45'function'45'into'45'if_3528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Bool ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_push'45'function'45'into'45'if_3528 = erased
