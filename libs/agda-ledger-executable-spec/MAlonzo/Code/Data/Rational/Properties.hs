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

module MAlonzo.Code.Data.Rational.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.Base
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp
import qualified MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Morphism.GroupMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.RingMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Integer.Coprimality
import qualified MAlonzo.Code.Data.Integer.GCD
import qualified MAlonzo.Code.Data.Integer.Properties
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Divisibility.Core
import qualified MAlonzo.Code.Data.Nat.GCD
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Rational.Base
import qualified MAlonzo.Code.Data.Rational.Unnormalised.Base
import qualified MAlonzo.Code.Data.Rational.Unnormalised.Properties
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.Morphism.RelMonomorphism
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Core
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core

-- Data.Rational.Properties._._DistributesOver_
d__DistributesOver__10 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d__DistributesOver__10 = erased
-- Data.Rational.Properties._._DistributesOverʳ_
d__DistributesOver'691'__12 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d__DistributesOver'691'__12 = erased
-- Data.Rational.Properties._._DistributesOverˡ_
d__DistributesOver'737'__14 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d__DistributesOver'737'__14 = erased
-- Data.Rational.Properties._.Associative
d_Associative_28 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_Associative_28 = erased
-- Data.Rational.Properties._.Commutative
d_Commutative_32 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_Commutative_32 = erased
-- Data.Rational.Properties._.Congruent₁
d_Congruent'8321'_34 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_Congruent'8321'_34 = erased
-- Data.Rational.Properties._.Identity
d_Identity_48 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_Identity_48 = erased
-- Data.Rational.Properties._.Inverse
d_Inverse_52 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_Inverse_52 = erased
-- Data.Rational.Properties._.LeftIdentity
d_LeftIdentity_74 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_LeftIdentity_74 = erased
-- Data.Rational.Properties._.LeftInverse
d_LeftInverse_76 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_LeftInverse_76 = erased
-- Data.Rational.Properties._.LeftZero
d_LeftZero_82 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_LeftZero_82 = erased
-- Data.Rational.Properties._.RightIdentity
d_RightIdentity_104 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_RightIdentity_104 = erased
-- Data.Rational.Properties._.RightInverse
d_RightInverse_106 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_RightInverse_106 = erased
-- Data.Rational.Properties._.RightZero
d_RightZero_112 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_RightZero_112 = erased
-- Data.Rational.Properties._.Zero
d_Zero_130 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  ()
d_Zero_130 = erased
-- Data.Rational.Properties._.IsAbelianGroup
d_IsAbelianGroup_134 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsAlternativeMagma
d_IsAlternativeMagma_136 a0 = ()
-- Data.Rational.Properties._.IsBand
d_IsBand_138 a0 = ()
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring
d_IsCancellativeCommutativeSemiring_140 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsCommutativeMagma
d_IsCommutativeMagma_142 a0 = ()
-- Data.Rational.Properties._.IsCommutativeMonoid
d_IsCommutativeMonoid_144 a0 a1 = ()
-- Data.Rational.Properties._.IsCommutativeRing
d_IsCommutativeRing_146 a0 a1 a2 a3 a4 = ()
-- Data.Rational.Properties._.IsCommutativeSemigroup
d_IsCommutativeSemigroup_148 a0 = ()
-- Data.Rational.Properties._.IsCommutativeSemiring
d_IsCommutativeSemiring_150 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne
d_IsCommutativeSemiringWithoutOne_152 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsFlexibleMagma
d_IsFlexibleMagma_154 a0 = ()
-- Data.Rational.Properties._.IsGroup
d_IsGroup_156 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid
d_IsIdempotentCommutativeMonoid_158 a0 a1 = ()
-- Data.Rational.Properties._.IsIdempotentMagma
d_IsIdempotentMagma_160 a0 = ()
-- Data.Rational.Properties._.IsIdempotentSemiring
d_IsIdempotentSemiring_162 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsInvertibleMagma
d_IsInvertibleMagma_164 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsInvertibleUnitalMagma
d_IsInvertibleUnitalMagma_166 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsKleeneAlgebra
d_IsKleeneAlgebra_168 a0 a1 a2 a3 a4 = ()
-- Data.Rational.Properties._.IsLeftBolLoop
d_IsLeftBolLoop_170 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsLoop
d_IsLoop_172 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsMagma
d_IsMagma_174 a0 = ()
-- Data.Rational.Properties._.IsMedialMagma
d_IsMedialMagma_176 a0 = ()
-- Data.Rational.Properties._.IsMiddleBolLoop
d_IsMiddleBolLoop_178 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsMonoid
d_IsMonoid_180 a0 a1 = ()
-- Data.Rational.Properties._.IsMoufangLoop
d_IsMoufangLoop_182 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsNearSemiring
d_IsNearSemiring_184 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsNearring
d_IsNearring_186 a0 a1 a2 a3 a4 = ()
-- Data.Rational.Properties._.IsNonAssociativeRing
d_IsNonAssociativeRing_188 a0 a1 a2 a3 a4 = ()
-- Data.Rational.Properties._.IsQuasigroup
d_IsQuasigroup_190 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsQuasiring
d_IsQuasiring_192 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsRightBolLoop
d_IsRightBolLoop_194 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsRing
d_IsRing_196 a0 a1 a2 a3 a4 = ()
-- Data.Rational.Properties._.IsRingWithoutOne
d_IsRingWithoutOne_198 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsSelectiveMagma
d_IsSelectiveMagma_200 a0 = ()
-- Data.Rational.Properties._.IsSemigroup
d_IsSemigroup_202 a0 = ()
-- Data.Rational.Properties._.IsSemimedialMagma
d_IsSemimedialMagma_204 a0 = ()
-- Data.Rational.Properties._.IsSemiring
d_IsSemiring_206 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero
d_IsSemiringWithoutAnnihilatingZero_208 a0 a1 a2 a3 = ()
-- Data.Rational.Properties._.IsSemiringWithoutOne
d_IsSemiringWithoutOne_210 a0 a1 a2 = ()
-- Data.Rational.Properties._.IsUnitalMagma
d_IsUnitalMagma_212 a0 a1 = ()
-- Data.Rational.Properties._.IsAbelianGroup.assoc
d_assoc_218 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_218 = erased
-- Data.Rational.Properties._.IsAbelianGroup.comm
d_comm_220 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_220 = erased
-- Data.Rational.Properties._.IsAbelianGroup.identity
d_identity_222 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Rational.Properties._.IsAbelianGroup.inverse
d_inverse_228 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Rational.Properties._.IsAbelianGroup.isEquivalence
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
-- Data.Rational.Properties._.IsAbelianGroup.isGroup
d_isGroup_242 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_242 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)
-- Data.Rational.Properties._.IsAbelianGroup.isMagma
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
-- Data.Rational.Properties._.IsAbelianGroup.isMonoid
d_isMonoid_250 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_250 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Data.Rational.Properties._.IsAbelianGroup.isSemigroup
d_isSemigroup_254 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0)))
-- Data.Rational.Properties._.IsAbelianGroup.⁻¹-cong
d_'8315''185''45'cong_272 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_272 = erased
-- Data.Rational.Properties._.IsAbelianGroup.∙-cong
d_'8729''45'cong_274 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_274 = erased
-- Data.Rational.Properties._.IsAlternativeMagma.alter
d_alter_282 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alter_282 v0
  = coe MAlonzo.Code.Algebra.Structures.d_alter_258 (coe v0)
-- Data.Rational.Properties._.IsAlternativeMagma.isEquivalence
d_isEquivalence_288 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_288 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0))
-- Data.Rational.Properties._.IsAlternativeMagma.isMagma
d_isMagma_290 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_290 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v0)
-- Data.Rational.Properties._.IsAlternativeMagma.∙-cong
d_'8729''45'cong_304 ::
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_304 = erased
-- Data.Rational.Properties._.IsBand.assoc
d_assoc_312 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_312 = erased
-- Data.Rational.Properties._.IsBand.idem
d_idem_314 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_314 = erased
-- Data.Rational.Properties._.IsBand.isEquivalence
d_isEquivalence_316 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_316 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)))
-- Data.Rational.Properties._.IsBand.isMagma
d_isMagma_318 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_318 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0))
-- Data.Rational.Properties._.IsBand.isSemigroup
d_isSemigroup_322 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_322 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0)
-- Data.Rational.Properties._.IsBand.∙-cong
d_'8729''45'cong_334 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_334 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.*-assoc
d_'42''45'assoc_342 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_342 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.*-cancelˡ-nonZero
d_'42''45'cancel'737''45'nonZero_344 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cancel'737''45'nonZero_344 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.*-comm
d_'42''45'comm_346 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_346 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.*-cong
d_'42''45'cong_348 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_348 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.*-identity
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.assoc
d_assoc_372 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_372 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.comm
d_comm_374 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_374 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.∙-cong
d_'8729''45'cong_376 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_376 = erased
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.identity
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.+-isCommutativeMonoid
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.isMagma
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.isMonoid
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.isSemigroup
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.distrib
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.isCommutativeSemiring
d_isCommutativeSemiring_408 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_408 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
      (coe v0)
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.isEquivalence
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.isSemiring
d_isSemiring_418 ::
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe v0))
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.isSemiringWithoutAnnihilatingZero
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
-- Data.Rational.Properties._.IsCancellativeCommutativeSemiring.zero
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
-- Data.Rational.Properties._.IsCommutativeMagma.comm
d_comm_442 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_442 = erased
-- Data.Rational.Properties._.IsCommutativeMagma.isEquivalence
d_isEquivalence_444 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_444 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0))
-- Data.Rational.Properties._.IsCommutativeMagma.isMagma
d_isMagma_446 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_446 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v0)
-- Data.Rational.Properties._.IsCommutativeMagma.∙-cong
d_'8729''45'cong_460 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_460 = erased
-- Data.Rational.Properties._.IsCommutativeMonoid.assoc
d_assoc_468 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_468 = erased
-- Data.Rational.Properties._.IsCommutativeMonoid.comm
d_comm_470 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_470 = erased
-- Data.Rational.Properties._.IsCommutativeMonoid.identity
d_identity_472 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_472 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Rational.Properties._.IsCommutativeMonoid.isEquivalence
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
-- Data.Rational.Properties._.IsCommutativeMonoid.isMagma
d_isMagma_484 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_484 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)))
-- Data.Rational.Properties._.IsCommutativeMonoid.isMonoid
d_isMonoid_486 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_486 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0)
-- Data.Rational.Properties._.IsCommutativeMonoid.isSemigroup
d_isSemigroup_490 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_490 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v0))
-- Data.Rational.Properties._.IsCommutativeMonoid.∙-cong
d_'8729''45'cong_504 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_504 = erased
-- Data.Rational.Properties._.IsCommutativeRing.*-assoc
d_'42''45'assoc_514 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_514 = erased
-- Data.Rational.Properties._.IsCommutativeRing.*-comm
d_'42''45'comm_516 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_516 = erased
-- Data.Rational.Properties._.IsCommutativeRing.*-cong
d_'42''45'cong_518 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_518 = erased
-- Data.Rational.Properties._.IsCommutativeRing.*-identity
d_'42''45'identity_524 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_524 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Rational.Properties._.IsCommutativeRing.assoc
d_assoc_542 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_542 = erased
-- Data.Rational.Properties._.IsCommutativeRing.comm
d_comm_544 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_544 = erased
-- Data.Rational.Properties._.IsCommutativeRing.∙-cong
d_'8729''45'cong_546 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_546 = erased
-- Data.Rational.Properties._.IsCommutativeRing.identity
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
-- Data.Rational.Properties._.IsCommutativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_558 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_558 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Rational.Properties._.IsCommutativeRing.isGroup
d_isGroup_566 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_566 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)))
-- Data.Rational.Properties._.IsCommutativeRing.isMagma
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
-- Data.Rational.Properties._.IsCommutativeRing.isMonoid
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
-- Data.Rational.Properties._.IsCommutativeRing.isSemigroup
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
-- Data.Rational.Properties._.IsCommutativeRing.⁻¹-cong
d_'8315''185''45'cong_580 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_580 = erased
-- Data.Rational.Properties._.IsCommutativeRing.inverse
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
-- Data.Rational.Properties._.IsCommutativeRing.distrib
d_distrib_588 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_588 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2426
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Rational.Properties._.IsCommutativeRing.isEquivalence
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
-- Data.Rational.Properties._.IsCommutativeRing.isRing
d_isRing_604 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_604 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0)
-- Data.Rational.Properties._.IsCommutativeRing.zero
d_zero_626 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_626 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2428
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v0))
-- Data.Rational.Properties._.IsCommutativeSemigroup.assoc
d_assoc_634 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_634 = erased
-- Data.Rational.Properties._.IsCommutativeSemigroup.comm
d_comm_636 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_636 = erased
-- Data.Rational.Properties._.IsCommutativeSemigroup.isEquivalence
d_isEquivalence_640 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_640 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)))
-- Data.Rational.Properties._.IsCommutativeSemigroup.isMagma
d_isMagma_642 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0))
-- Data.Rational.Properties._.IsCommutativeSemigroup.isSemigroup
d_isSemigroup_646 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_646 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v0)
-- Data.Rational.Properties._.IsCommutativeSemigroup.∙-cong
d_'8729''45'cong_658 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_658 = erased
-- Data.Rational.Properties._.IsCommutativeSemiring.*-assoc
d_'42''45'assoc_666 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_666 = erased
-- Data.Rational.Properties._.IsCommutativeSemiring.*-comm
d_'42''45'comm_668 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_668 = erased
-- Data.Rational.Properties._.IsCommutativeSemiring.*-cong
d_'42''45'cong_670 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_670 = erased
-- Data.Rational.Properties._.IsCommutativeSemiring.*-identity
d_'42''45'identity_676 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_676 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Rational.Properties._.IsCommutativeSemiring.assoc
d_assoc_694 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_694 = erased
-- Data.Rational.Properties._.IsCommutativeSemiring.comm
d_comm_696 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_696 = erased
-- Data.Rational.Properties._.IsCommutativeSemiring.∙-cong
d_'8729''45'cong_698 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_698 = erased
-- Data.Rational.Properties._.IsCommutativeSemiring.identity
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
-- Data.Rational.Properties._.IsCommutativeSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_712 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_712 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Rational.Properties._.IsCommutativeSemiring.isMagma
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
-- Data.Rational.Properties._.IsCommutativeSemiring.isMonoid
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
-- Data.Rational.Properties._.IsCommutativeSemiring.isSemigroup
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
-- Data.Rational.Properties._.IsCommutativeSemiring.distrib
d_distrib_724 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_724 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)))
-- Data.Rational.Properties._.IsCommutativeSemiring.isEquivalence
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
-- Data.Rational.Properties._.IsCommutativeSemiring.isSemiring
d_isSemiring_738 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_738 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0)
-- Data.Rational.Properties._.IsCommutativeSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_740 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_740 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Rational.Properties._.IsCommutativeSemiring.zero
d_zero_754 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_754 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v0))
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.*-assoc
d_'42''45'assoc_762 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_762 = erased
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.*-comm
d_'42''45'comm_764 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_764 = erased
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.*-cong
d_'42''45'cong_766 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_766 = erased
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.comm
d_comm_780 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_780 = erased
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_784 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_784 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.isMonoid
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
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.distrib
d_distrib_790 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_790 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1166
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.isEquivalence
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
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.isSemiringWithoutOne
d_isSemiringWithoutOne_796 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_796 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
      (coe v0)
-- Data.Rational.Properties._.IsCommutativeSemiringWithoutOne.zero
d_zero_798 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_798 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1168
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v0))
-- Data.Rational.Properties._.IsFlexibleMagma.flex
d_flex_806 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_flex_806 = erased
-- Data.Rational.Properties._.IsFlexibleMagma.isEquivalence
d_isEquivalence_808 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_808 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0))
-- Data.Rational.Properties._.IsFlexibleMagma.isMagma
d_isMagma_810 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_810 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v0)
-- Data.Rational.Properties._.IsFlexibleMagma.∙-cong
d_'8729''45'cong_824 ::
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_824 = erased
-- Data.Rational.Properties._.IsGroup.assoc
d_assoc_834 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_834 = erased
-- Data.Rational.Properties._.IsGroup.identity
d_identity_836 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_836 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Rational.Properties._.IsGroup.inverse
d_inverse_842 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_842 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_904 (coe v0)
-- Data.Rational.Properties._.IsGroup.isEquivalence
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
-- Data.Rational.Properties._.IsGroup.isMagma
d_isMagma_854 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_854 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)))
-- Data.Rational.Properties._.IsGroup.isMonoid
d_isMonoid_856 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_856 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0)
-- Data.Rational.Properties._.IsGroup.isSemigroup
d_isSemigroup_860 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_860 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v0))
-- Data.Rational.Properties._.IsGroup.⁻¹-cong
d_'8315''185''45'cong_878 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_878 = erased
-- Data.Rational.Properties._.IsGroup.∙-cong
d_'8729''45'cong_880 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_880 = erased
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.assoc
d_assoc_888 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_888 = erased
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.comm
d_comm_890 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_890 = erased
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.idem
d_idem_892 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_892 = erased
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.identity
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
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.isCommutativeMonoid
d_isCommutativeMonoid_904 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_904 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0)
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.isEquivalence
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
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.isMagma
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
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.isMonoid
d_isMonoid_912 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_912 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.isSemigroup
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
-- Data.Rational.Properties._.IsIdempotentCommutativeMonoid.∙-cong
d_'8729''45'cong_930 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_930 = erased
-- Data.Rational.Properties._.IsIdempotentMagma.idem
d_idem_938 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_idem_938 = erased
-- Data.Rational.Properties._.IsIdempotentMagma.isEquivalence
d_isEquivalence_940 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_940 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0))
-- Data.Rational.Properties._.IsIdempotentMagma.isMagma
d_isMagma_942 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_942 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v0)
-- Data.Rational.Properties._.IsIdempotentMagma.∙-cong
d_'8729''45'cong_956 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_956 = erased
-- Data.Rational.Properties._.IsIdempotentSemiring.*-assoc
d_'42''45'assoc_964 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_964 = erased
-- Data.Rational.Properties._.IsIdempotentSemiring.*-cong
d_'42''45'cong_966 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_966 = erased
-- Data.Rational.Properties._.IsIdempotentSemiring.*-identity
d_'42''45'identity_972 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_972 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Rational.Properties._.IsIdempotentSemiring.assoc
d_assoc_984 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_984 = erased
-- Data.Rational.Properties._.IsIdempotentSemiring.comm
d_comm_986 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_986 = erased
-- Data.Rational.Properties._.IsIdempotentSemiring.∙-cong
d_'8729''45'cong_988 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_988 = erased
-- Data.Rational.Properties._.IsIdempotentSemiring.+-idem
d_'43''45'idem_994 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_994 = erased
-- Data.Rational.Properties._.IsIdempotentSemiring.identity
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
-- Data.Rational.Properties._.IsIdempotentSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1004 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1004 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Rational.Properties._.IsIdempotentSemiring.isMagma
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
-- Data.Rational.Properties._.IsIdempotentSemiring.isMonoid
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
-- Data.Rational.Properties._.IsIdempotentSemiring.isSemigroup
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
-- Data.Rational.Properties._.IsIdempotentSemiring.distrib
d_distrib_1016 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1016 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)))
-- Data.Rational.Properties._.IsIdempotentSemiring.isEquivalence
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
-- Data.Rational.Properties._.IsIdempotentSemiring.isSemiring
d_isSemiring_1028 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1028 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0)
-- Data.Rational.Properties._.IsIdempotentSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1030 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1030 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Rational.Properties._.IsIdempotentSemiring.zero
d_zero_1044 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v0))
-- Data.Rational.Properties._.IsInvertibleMagma.inverse
d_inverse_1052 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1052 v0
  = coe MAlonzo.Code.Algebra.Structures.d_inverse_792 (coe v0)
-- Data.Rational.Properties._.IsInvertibleMagma.isEquivalence
d_isEquivalence_1058 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0))
-- Data.Rational.Properties._.IsInvertibleMagma.isMagma
d_isMagma_1060 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1060 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v0)
-- Data.Rational.Properties._.IsInvertibleMagma.⁻¹-cong
d_'8315''185''45'cong_1074 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1074 = erased
-- Data.Rational.Properties._.IsInvertibleMagma.∙-cong
d_'8729''45'cong_1076 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1076 = erased
-- Data.Rational.Properties._.IsInvertibleUnitalMagma.identity
d_identity_1084 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1084 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_842 (coe v0)
-- Data.Rational.Properties._.IsInvertibleUnitalMagma.inverse
d_inverse_1090 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1090 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_792
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Rational.Properties._.IsInvertibleUnitalMagma.isEquivalence
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
-- Data.Rational.Properties._.IsInvertibleUnitalMagma.isInvertibleMagma
d_isInvertibleMagma_1098 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1098 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0)
-- Data.Rational.Properties._.IsInvertibleUnitalMagma.isMagma
d_isMagma_1100 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1100 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_790
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v0))
-- Data.Rational.Properties._.IsInvertibleUnitalMagma.⁻¹-cong
d_'8315''185''45'cong_1116 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1116 = erased
-- Data.Rational.Properties._.IsInvertibleUnitalMagma.∙-cong
d_'8729''45'cong_1118 ::
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1118 = erased
-- Data.Rational.Properties._.IsKleeneAlgebra.*-assoc
d_'42''45'assoc_1126 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1126 = erased
-- Data.Rational.Properties._.IsKleeneAlgebra.*-cong
d_'42''45'cong_1128 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1128 = erased
-- Data.Rational.Properties._.IsKleeneAlgebra.*-identity
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
-- Data.Rational.Properties._.IsKleeneAlgebra.assoc
d_assoc_1146 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1146 = erased
-- Data.Rational.Properties._.IsKleeneAlgebra.comm
d_comm_1148 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1148 = erased
-- Data.Rational.Properties._.IsKleeneAlgebra.∙-cong
d_'8729''45'cong_1150 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1150 = erased
-- Data.Rational.Properties._.IsKleeneAlgebra.+-idem
d_'43''45'idem_1156 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'idem_1156 = erased
-- Data.Rational.Properties._.IsKleeneAlgebra.identity
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
-- Data.Rational.Properties._.IsKleeneAlgebra.+-isCommutativeMonoid
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
-- Data.Rational.Properties._.IsKleeneAlgebra.isMagma
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
-- Data.Rational.Properties._.IsKleeneAlgebra.isMonoid
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
-- Data.Rational.Properties._.IsKleeneAlgebra.isSemigroup
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
-- Data.Rational.Properties._.IsKleeneAlgebra.distrib
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
-- Data.Rational.Properties._.IsKleeneAlgebra.isEquivalence
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
-- Data.Rational.Properties._.IsKleeneAlgebra.isIdempotentSemiring
d_isIdempotentSemiring_1186 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_1186 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
      (coe v0)
-- Data.Rational.Properties._.IsKleeneAlgebra.isSemiring
d_isSemiring_1192 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1192 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
      (coe
         MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe v0))
-- Data.Rational.Properties._.IsKleeneAlgebra.isSemiringWithoutAnnihilatingZero
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
-- Data.Rational.Properties._.IsKleeneAlgebra.starDestructive
d_starDestructive_1204 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starDestructive_1204 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_starDestructive_1856 (coe v0)
-- Data.Rational.Properties._.IsKleeneAlgebra.starExpansive
d_starExpansive_1210 ::
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starExpansive_1210 v0
  = coe MAlonzo.Code.Algebra.Structures.d_starExpansive_1854 (coe v0)
-- Data.Rational.Properties._.IsKleeneAlgebra.zero
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
-- Data.Rational.Properties._.IsLeftBolLoop.//-cong
d_'47''47''45'cong_1228 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1228 = erased
-- Data.Rational.Properties._.IsLeftBolLoop.\\-cong
d_'92''92''45'cong_1234 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1234 = erased
-- Data.Rational.Properties._.IsLeftBolLoop.identity
d_identity_1240 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Rational.Properties._.IsLeftBolLoop.isEquivalence
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
-- Data.Rational.Properties._.IsLeftBolLoop.isLoop
d_isLoop_1248 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1248 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)
-- Data.Rational.Properties._.IsLeftBolLoop.isMagma
d_isMagma_1250 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1250 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Rational.Properties._.IsLeftBolLoop.isQuasigroup
d_isQuasigroup_1254 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0))
-- Data.Rational.Properties._.IsLeftBolLoop.leftBol
d_leftBol_1256 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1256 = erased
-- Data.Rational.Properties._.IsLeftBolLoop.leftDivides
d_leftDivides_1258 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1258 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Rational.Properties._.IsLeftBolLoop.rightDivides
d_rightDivides_1268 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1268 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v0)))
-- Data.Rational.Properties._.IsLeftBolLoop.∙-cong
d_'8729''45'cong_1280 ::
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1280 = erased
-- Data.Rational.Properties._.IsLoop.//-cong
d_'47''47''45'cong_1288 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1288 = erased
-- Data.Rational.Properties._.IsLoop.\\-cong
d_'92''92''45'cong_1294 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1294 = erased
-- Data.Rational.Properties._.IsLoop.identity
d_identity_1300 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1300 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2784 (coe v0)
-- Data.Rational.Properties._.IsLoop.isEquivalence
d_isEquivalence_1306 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)))
-- Data.Rational.Properties._.IsLoop.isMagma
d_isMagma_1308 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1308 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Rational.Properties._.IsLoop.isQuasigroup
d_isQuasigroup_1312 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1312 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0)
-- Data.Rational.Properties._.IsLoop.leftDivides
d_leftDivides_1314 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1314 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Rational.Properties._.IsLoop.rightDivides
d_rightDivides_1324 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1324 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v0))
-- Data.Rational.Properties._.IsLoop.∙-cong
d_'8729''45'cong_1336 ::
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1336 = erased
-- Data.Rational.Properties._.IsMagma.isEquivalence
d_isEquivalence_1344 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1344 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v0)
-- Data.Rational.Properties._.IsMagma.∙-cong
d_'8729''45'cong_1358 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1358 = erased
-- Data.Rational.Properties._.IsMedialMagma.isEquivalence
d_isEquivalence_1366 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1366 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0))
-- Data.Rational.Properties._.IsMedialMagma.isMagma
d_isMagma_1368 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1368 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v0)
-- Data.Rational.Properties._.IsMedialMagma.medial
d_medial_1372 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_medial_1372 = erased
-- Data.Rational.Properties._.IsMedialMagma.∙-cong
d_'8729''45'cong_1384 ::
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1384 = erased
-- Data.Rational.Properties._.IsMiddleBolLoop.//-cong
d_'47''47''45'cong_1392 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1392 = erased
-- Data.Rational.Properties._.IsMiddleBolLoop.\\-cong
d_'92''92''45'cong_1398 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1398 = erased
-- Data.Rational.Properties._.IsMiddleBolLoop.identity
d_identity_1404 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1404 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Rational.Properties._.IsMiddleBolLoop.isEquivalence
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
-- Data.Rational.Properties._.IsMiddleBolLoop.isLoop
d_isLoop_1412 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1412 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)
-- Data.Rational.Properties._.IsMiddleBolLoop.isMagma
d_isMagma_1414 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1414 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Rational.Properties._.IsMiddleBolLoop.isQuasigroup
d_isQuasigroup_1418 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0))
-- Data.Rational.Properties._.IsMiddleBolLoop.leftDivides
d_leftDivides_1420 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1420 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Rational.Properties._.IsMiddleBolLoop.middleBol
d_middleBol_1426 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_middleBol_1426 = erased
-- Data.Rational.Properties._.IsMiddleBolLoop.rightDivides
d_rightDivides_1432 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1432 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v0)))
-- Data.Rational.Properties._.IsMiddleBolLoop.∙-cong
d_'8729''45'cong_1444 ::
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1444 = erased
-- Data.Rational.Properties._.IsMonoid.assoc
d_assoc_1452 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1452 = erased
-- Data.Rational.Properties._.IsMonoid.identity
d_identity_1454 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1454 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_612 (coe v0)
-- Data.Rational.Properties._.IsMonoid.isEquivalence
d_isEquivalence_1460 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1460 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)))
-- Data.Rational.Properties._.IsMonoid.isMagma
d_isMagma_1462 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1462 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0))
-- Data.Rational.Properties._.IsMonoid.isSemigroup
d_isSemigroup_1466 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1466 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0)
-- Data.Rational.Properties._.IsMonoid.∙-cong
d_'8729''45'cong_1480 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1480 = erased
-- Data.Rational.Properties._.IsMoufangLoop.//-cong
d_'47''47''45'cong_1488 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1488 = erased
-- Data.Rational.Properties._.IsMoufangLoop.\\-cong
d_'92''92''45'cong_1494 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1494 = erased
-- Data.Rational.Properties._.IsMoufangLoop.identical
d_identical_1500 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identical_1500 = erased
-- Data.Rational.Properties._.IsMoufangLoop.identity
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
-- Data.Rational.Properties._.IsMoufangLoop.isEquivalence
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
-- Data.Rational.Properties._.IsMoufangLoop.isLeftBolLoop
d_isLeftBolLoop_1510 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
d_isLeftBolLoop_1510 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0)
-- Data.Rational.Properties._.IsMoufangLoop.isLoop
d_isLoop_1512 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1512 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2860
      (coe MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v0))
-- Data.Rational.Properties._.IsMoufangLoop.isMagma
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
-- Data.Rational.Properties._.IsMoufangLoop.isQuasigroup
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
-- Data.Rational.Properties._.IsMoufangLoop.leftBol
d_leftBol_1520 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_leftBol_1520 = erased
-- Data.Rational.Properties._.IsMoufangLoop.leftDivides
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
-- Data.Rational.Properties._.IsMoufangLoop.rightBol
d_rightBol_1532 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1532 = erased
-- Data.Rational.Properties._.IsMoufangLoop.rightDivides
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
-- Data.Rational.Properties._.IsMoufangLoop.∙-cong
d_'8729''45'cong_1546 ::
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1546 = erased
-- Data.Rational.Properties._.IsNearSemiring.*-assoc
d_'42''45'assoc_1554 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1554 = erased
-- Data.Rational.Properties._.IsNearSemiring.*-cong
d_'42''45'cong_1556 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1556 = erased
-- Data.Rational.Properties._.IsNearSemiring.assoc
d_assoc_1566 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1566 = erased
-- Data.Rational.Properties._.IsNearSemiring.∙-cong
d_'8729''45'cong_1568 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1568 = erased
-- Data.Rational.Properties._.IsNearSemiring.identity
d_identity_1574 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Rational.Properties._.IsNearSemiring.isMagma
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
-- Data.Rational.Properties._.IsNearSemiring.+-isMonoid
d_'43''45'isMonoid_1582 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0)
-- Data.Rational.Properties._.IsNearSemiring.isSemigroup
d_isSemigroup_1584 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1584 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v0))
-- Data.Rational.Properties._.IsNearSemiring.distribʳ
d_distrib'691'_1588 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691'_1588 = erased
-- Data.Rational.Properties._.IsNearSemiring.isEquivalence
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
-- Data.Rational.Properties._.IsNearSemiring.zeroˡ
d_zero'737'_1604 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zero'737'_1604 = erased
-- Data.Rational.Properties._.IsNearring.*-assoc
d_'42''45'assoc_1608 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1608 = erased
-- Data.Rational.Properties._.IsNearring.*-cong
d_'42''45'cong_1610 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1610 = erased
-- Data.Rational.Properties._.IsNearring.*-identity
d_'42''45'identity_1616 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1616 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Rational.Properties._.IsNearring.assoc
d_assoc_1628 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1628 = erased
-- Data.Rational.Properties._.IsNearring.∙-cong
d_'8729''45'cong_1630 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1630 = erased
-- Data.Rational.Properties._.IsNearring.identity
d_identity_1636 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1636 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Rational.Properties._.IsNearring.+-inverse
d_'43''45'inverse_1642 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_1642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'inverse_2314 (coe v0)
-- Data.Rational.Properties._.IsNearring.isMagma
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
-- Data.Rational.Properties._.IsNearring.+-isMonoid
d_'43''45'isMonoid_1650 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1650 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Rational.Properties._.IsNearring.isSemigroup
d_isSemigroup_1652 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1652 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)))
-- Data.Rational.Properties._.IsNearring.distrib
d_distrib_1656 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1656 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1992
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Rational.Properties._.IsNearring.isEquivalence
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
-- Data.Rational.Properties._.IsNearring.isQuasiring
d_isQuasiring_1662 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
d_isQuasiring_1662 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0)
-- Data.Rational.Properties._.IsNearring.zero
d_zero_1674 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1674 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1994
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v0))
-- Data.Rational.Properties._.IsNearring.⁻¹-cong
d_'8315''185''45'cong_1676 ::
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1676 = erased
-- Data.Rational.Properties._.IsNonAssociativeRing.*-cong
d_'42''45'cong_1682 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1682 = erased
-- Data.Rational.Properties._.IsNonAssociativeRing.assoc
d_assoc_1690 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1690 = erased
-- Data.Rational.Properties._.IsNonAssociativeRing.comm
d_comm_1692 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1692 = erased
-- Data.Rational.Properties._.IsNonAssociativeRing.∙-cong
d_'8729''45'cong_1694 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1694 = erased
-- Data.Rational.Properties._.IsNonAssociativeRing.identity
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
-- Data.Rational.Properties._.IsNonAssociativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1706 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1706 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
      (coe v0)
-- Data.Rational.Properties._.IsNonAssociativeRing.isGroup
d_isGroup_1714 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1714 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe v0))
-- Data.Rational.Properties._.IsNonAssociativeRing.isMagma
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
-- Data.Rational.Properties._.IsNonAssociativeRing.isMonoid
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
-- Data.Rational.Properties._.IsNonAssociativeRing.isSemigroup
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
-- Data.Rational.Properties._.IsNonAssociativeRing.⁻¹-cong
d_'8315''185''45'cong_1728 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1728 = erased
-- Data.Rational.Properties._.IsNonAssociativeRing.inverse
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
-- Data.Rational.Properties._.IsNonAssociativeRing.distrib
d_distrib_1736 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1736 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2208 (coe v0)
-- Data.Rational.Properties._.IsNonAssociativeRing.identity
d_identity_1738 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1738 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_2206 (coe v0)
-- Data.Rational.Properties._.IsNonAssociativeRing.isEquivalence
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
-- Data.Rational.Properties._.IsNonAssociativeRing.zero
d_zero_1758 ::
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1758 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2210 (coe v0)
-- Data.Rational.Properties._.IsQuasigroup.//-cong
d_'47''47''45'cong_1762 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1762 = erased
-- Data.Rational.Properties._.IsQuasigroup.\\-cong
d_'92''92''45'cong_1768 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1768 = erased
-- Data.Rational.Properties._.IsQuasigroup.isEquivalence
d_isEquivalence_1774 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1774 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0))
-- Data.Rational.Properties._.IsQuasigroup.isMagma
d_isMagma_1776 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1776 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v0)
-- Data.Rational.Properties._.IsQuasigroup.leftDivides
d_leftDivides_1780 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1780 v0
  = coe MAlonzo.Code.Algebra.Structures.d_leftDivides_2710 (coe v0)
-- Data.Rational.Properties._.IsQuasigroup.rightDivides
d_rightDivides_1790 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1790 v0
  = coe MAlonzo.Code.Algebra.Structures.d_rightDivides_2712 (coe v0)
-- Data.Rational.Properties._.IsQuasigroup.∙-cong
d_'8729''45'cong_1802 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1802 = erased
-- Data.Rational.Properties._.IsQuasiring.*-assoc
d_'42''45'assoc_1810 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1810 = erased
-- Data.Rational.Properties._.IsQuasiring.*-cong
d_'42''45'cong_1812 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1812 = erased
-- Data.Rational.Properties._.IsQuasiring.*-identity
d_'42''45'identity_1818 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1818 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990 (coe v0)
-- Data.Rational.Properties._.IsQuasiring.assoc
d_assoc_1830 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1830 = erased
-- Data.Rational.Properties._.IsQuasiring.∙-cong
d_'8729''45'cong_1832 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1832 = erased
-- Data.Rational.Properties._.IsQuasiring.identity
d_identity_1838 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1838 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Rational.Properties._.IsQuasiring.isMagma
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
-- Data.Rational.Properties._.IsQuasiring.+-isMonoid
d_'43''45'isMonoid_1846 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1846 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0)
-- Data.Rational.Properties._.IsQuasiring.isSemigroup
d_isSemigroup_1848 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v0))
-- Data.Rational.Properties._.IsQuasiring.distrib
d_distrib_1852 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1852 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1992 (coe v0)
-- Data.Rational.Properties._.IsQuasiring.isEquivalence
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
-- Data.Rational.Properties._.IsQuasiring.zero
d_zero_1868 ::
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1868 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1994 (coe v0)
-- Data.Rational.Properties._.IsRightBolLoop.//-cong
d_'47''47''45'cong_1872 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''47''45'cong_1872 = erased
-- Data.Rational.Properties._.IsRightBolLoop.\\-cong
d_'92''92''45'cong_1878 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'92''92''45'cong_1878 = erased
-- Data.Rational.Properties._.IsRightBolLoop.identity
d_identity_1884 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1884 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Rational.Properties._.IsRightBolLoop.isEquivalence
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
-- Data.Rational.Properties._.IsRightBolLoop.isLoop
d_isLoop_1892 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_1892 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)
-- Data.Rational.Properties._.IsRightBolLoop.isMagma
d_isMagma_1894 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Rational.Properties._.IsRightBolLoop.isQuasigroup
d_isQuasigroup_1898 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_1898 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0))
-- Data.Rational.Properties._.IsRightBolLoop.leftDivides
d_leftDivides_1900 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_1900 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Rational.Properties._.IsRightBolLoop.rightBol
d_rightBol_1910 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rightBol_1910 = erased
-- Data.Rational.Properties._.IsRightBolLoop.rightDivides
d_rightDivides_1912 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_1912 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v0)))
-- Data.Rational.Properties._.IsRightBolLoop.∙-cong
d_'8729''45'cong_1924 ::
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1924 = erased
-- Data.Rational.Properties._.IsRing.*-assoc
d_'42''45'assoc_1934 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_1934 = erased
-- Data.Rational.Properties._.IsRing.*-cong
d_'42''45'cong_1936 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_1936 = erased
-- Data.Rational.Properties._.IsRing.*-identity
d_'42''45'identity_1942 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1942 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424 (coe v0)
-- Data.Rational.Properties._.IsRing.assoc
d_assoc_1954 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_1954 = erased
-- Data.Rational.Properties._.IsRing.comm
d_comm_1956 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_1956 = erased
-- Data.Rational.Properties._.IsRing.∙-cong
d_'8729''45'cong_1958 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_1958 = erased
-- Data.Rational.Properties._.IsRing.identity
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
-- Data.Rational.Properties._.IsRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_1970 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_1970 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe v0)
-- Data.Rational.Properties._.IsRing.isGroup
d_isGroup_1978 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1978 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe v0))
-- Data.Rational.Properties._.IsRing.isMagma
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
-- Data.Rational.Properties._.IsRing.isMonoid
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
-- Data.Rational.Properties._.IsRing.isSemigroup
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
-- Data.Rational.Properties._.IsRing.⁻¹-cong
d_'8315''185''45'cong_1992 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_1992 = erased
-- Data.Rational.Properties._.IsRing.inverse
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
-- Data.Rational.Properties._.IsRing.distrib
d_distrib_2000 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2000 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2426 (coe v0)
-- Data.Rational.Properties._.IsRing.isEquivalence
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
-- Data.Rational.Properties._.IsRing.zero
d_zero_2032 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2032 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2428 (coe v0)
-- Data.Rational.Properties._.IsRingWithoutOne.*-assoc
d_'42''45'assoc_2042 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2042 = erased
-- Data.Rational.Properties._.IsRingWithoutOne.*-cong
d_'42''45'cong_2044 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2044 = erased
-- Data.Rational.Properties._.IsRingWithoutOne.assoc
d_assoc_2054 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2054 = erased
-- Data.Rational.Properties._.IsRingWithoutOne.comm
d_comm_2056 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2056 = erased
-- Data.Rational.Properties._.IsRingWithoutOne.∙-cong
d_'8729''45'cong_2058 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2058 = erased
-- Data.Rational.Properties._.IsRingWithoutOne.identity
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
-- Data.Rational.Properties._.IsRingWithoutOne.+-isAbelianGroup
d_'43''45'isAbelianGroup_2070 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2070 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
      (coe v0)
-- Data.Rational.Properties._.IsRingWithoutOne.isGroup
d_isGroup_2078 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_2078 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe v0))
-- Data.Rational.Properties._.IsRingWithoutOne.isMagma
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
-- Data.Rational.Properties._.IsRingWithoutOne.isMonoid
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
-- Data.Rational.Properties._.IsRingWithoutOne.isSemigroup
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
-- Data.Rational.Properties._.IsRingWithoutOne.⁻¹-cong
d_'8315''185''45'cong_2092 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_2092 = erased
-- Data.Rational.Properties._.IsRingWithoutOne.inverse
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
-- Data.Rational.Properties._.IsRingWithoutOne.distrib
d_distrib_2100 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2100 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_2082 (coe v0)
-- Data.Rational.Properties._.IsRingWithoutOne.isEquivalence
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
-- Data.Rational.Properties._.IsRingWithoutOne.zero
d_zero_2124 ::
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2124 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_2084 (coe v0)
-- Data.Rational.Properties._.IsSelectiveMagma.isEquivalence
d_isEquivalence_2132 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0))
-- Data.Rational.Properties._.IsSelectiveMagma.isMagma
d_isMagma_2134 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2134 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v0)
-- Data.Rational.Properties._.IsSelectiveMagma.sel
d_sel_2142 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_2142 v0
  = coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0)
-- Data.Rational.Properties._.IsSelectiveMagma.∙-cong
d_'8729''45'cong_2150 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2150 = erased
-- Data.Rational.Properties._.IsSemigroup.assoc
d_assoc_2158 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2158 = erased
-- Data.Rational.Properties._.IsSemigroup.isEquivalence
d_isEquivalence_2160 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2160 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0))
-- Data.Rational.Properties._.IsSemigroup.isMagma
d_isMagma_2162 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2162 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v0)
-- Data.Rational.Properties._.IsSemigroup.∙-cong
d_'8729''45'cong_2176 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2176 = erased
-- Data.Rational.Properties._.IsSemimedialMagma.isEquivalence
d_isEquivalence_2184 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2184 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0))
-- Data.Rational.Properties._.IsSemimedialMagma.isMagma
d_isMagma_2186 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2186 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v0)
-- Data.Rational.Properties._.IsSemimedialMagma.semiMedial
d_semiMedial_2194 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semiMedial_2194 v0
  = coe MAlonzo.Code.Algebra.Structures.d_semiMedial_370 (coe v0)
-- Data.Rational.Properties._.IsSemimedialMagma.∙-cong
d_'8729''45'cong_2206 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2206 = erased
-- Data.Rational.Properties._.IsSemiring.*-assoc
d_'42''45'assoc_2214 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2214 = erased
-- Data.Rational.Properties._.IsSemiring.*-cong
d_'42''45'cong_2216 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2216 = erased
-- Data.Rational.Properties._.IsSemiring.*-identity
d_'42''45'identity_2222 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Rational.Properties._.IsSemiring.assoc
d_assoc_2234 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2234 = erased
-- Data.Rational.Properties._.IsSemiring.comm
d_comm_2236 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2236 = erased
-- Data.Rational.Properties._.IsSemiring.∙-cong
d_'8729''45'cong_2238 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2238 = erased
-- Data.Rational.Properties._.IsSemiring.identity
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
-- Data.Rational.Properties._.IsSemiring.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2252 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2252 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Rational.Properties._.IsSemiring.isMagma
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
-- Data.Rational.Properties._.IsSemiring.isMonoid
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
-- Data.Rational.Properties._.IsSemiring.isSemigroup
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
-- Data.Rational.Properties._.IsSemiring.distrib
d_distrib_2264 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v0))
-- Data.Rational.Properties._.IsSemiring.isEquivalence
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
-- Data.Rational.Properties._.IsSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2276 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2276 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe v0)
-- Data.Rational.Properties._.IsSemiring.zero
d_zero_2290 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2290 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1388 (coe v0)
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.*-assoc
d_'42''45'assoc_2298 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2298 = erased
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.*-cong
d_'42''45'cong_2300 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2300 = erased
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.*-identity
d_'42''45'identity_2306 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296 (coe v0)
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.assoc
d_assoc_2318 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_2318 = erased
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.comm
d_comm_2320 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2320 = erased
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.∙-cong
d_'8729''45'cong_2322 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2322 = erased
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.identity
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
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2336 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe v0)
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.isMagma
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
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.isMonoid
d_isMonoid_2342 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2342 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v0))
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.isSemigroup
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
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.distrib
d_distrib_2348 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2348 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1298 (coe v0)
-- Data.Rational.Properties._.IsSemiringWithoutAnnihilatingZero.isEquivalence
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
-- Data.Rational.Properties._.IsSemiringWithoutOne.*-assoc
d_'42''45'assoc_2370 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_2370 = erased
-- Data.Rational.Properties._.IsSemiringWithoutOne.*-cong
d_'42''45'cong_2372 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'cong_2372 = erased
-- Data.Rational.Properties._.IsSemiringWithoutOne.comm
d_comm_2382 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_2382 = erased
-- Data.Rational.Properties._.IsSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2386 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2386 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe v0)
-- Data.Rational.Properties._.IsSemiringWithoutOne.isMonoid
d_isMonoid_2390 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2390 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe v0))
-- Data.Rational.Properties._.IsSemiringWithoutOne.distrib
d_distrib_2392 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2392 v0
  = coe MAlonzo.Code.Algebra.Structures.d_distrib_1166 (coe v0)
-- Data.Rational.Properties._.IsSemiringWithoutOne.isEquivalence
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
-- Data.Rational.Properties._.IsSemiringWithoutOne.zero
d_zero_2398 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2398 v0
  = coe MAlonzo.Code.Algebra.Structures.d_zero_1168 (coe v0)
-- Data.Rational.Properties._.IsUnitalMagma.identity
d_identity_2406 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2406 v0
  = coe MAlonzo.Code.Algebra.Structures.d_identity_568 (coe v0)
-- Data.Rational.Properties._.IsUnitalMagma.isEquivalence
d_isEquivalence_2412 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0))
-- Data.Rational.Properties._.IsUnitalMagma.isMagma
d_isMagma_2414 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2414 v0
  = coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v0)
-- Data.Rational.Properties._.IsUnitalMagma.∙-cong
d_'8729''45'cong_2428 ::
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8729''45'cong_2428 = erased
-- Data.Rational.Properties.mkℚ-cong
d_mkℚ'45'cong_2452 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mkℚ'45'cong_2452 = erased
-- Data.Rational.Properties.mkℚ-injective
d_mkℚ'45'injective_2466 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_mkℚ'45'injective_2466 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_mkℚ'45'injective_2466
du_mkℚ'45'injective_2466 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_mkℚ'45'injective_2466
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Properties._≟_
d__'8799'__2468 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__2468 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    (coe MAlonzo.Code.Data.Product.Base.du_uncurry_220 erased)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                       (coe
                          MAlonzo.Code.Data.Integer.Properties.d__'8799'__2476 (coe v2)
                          (coe v5))
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464 (coe v3)
                          (coe v6)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.≡-setoid
d_'8801''45'setoid_2478 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''45'setoid_2478
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402
-- Data.Rational.Properties.≡-decSetoid
d_'8801''45'decSetoid_2480 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8801''45'decSetoid_2480
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
      (coe d__'8799'__2468)
-- Data.Rational.Properties.mkℚ+-cong
d_mkℚ'43''45'cong_2498 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mkℚ'43''45'cong_2498 = erased
-- Data.Rational.Properties.mkℚ+-injective
d_mkℚ'43''45'injective_2516 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_mkℚ'43''45'injective_2516 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
  = du_mkℚ'43''45'injective_2516
du_mkℚ'43''45'injective_2516 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_mkℚ'43''45'injective_2516
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Properties.↥-mkℚ+
d_'8613''45'mkℚ'43'_2526 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''45'mkℚ'43'_2526 = erased
-- Data.Rational.Properties.↧-mkℚ+
d_'8615''45'mkℚ'43'_2540 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''45'mkℚ'43'_2540 = erased
-- Data.Rational.Properties.mkℚ+-nonNeg
d_mkℚ'43''45'nonNeg_2554 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_mkℚ'43''45'nonNeg_2554 ~v0 ~v1 ~v2 ~v3
  = du_mkℚ'43''45'nonNeg_2554
du_mkℚ'43''45'nonNeg_2554 ::
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_mkℚ'43''45'nonNeg_2554
  = coe
      MAlonzo.Code.Data.Integer.Base.C_NonNegative'46'constructor_1353
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Rational.Properties.mkℚ+-pos
d_mkℚ'43''45'pos_2570 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_mkℚ'43''45'pos_2570 ~v0 ~v1 ~v2 ~v3 ~v4 = du_mkℚ'43''45'pos_2570
du_mkℚ'43''45'pos_2570 ::
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_mkℚ'43''45'pos_2570
  = coe
      MAlonzo.Code.Data.Integer.Base.C_Positive'46'constructor_1295
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Rational.Properties.≡⇒≃
d_'8801''8658''8771'_2576 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8801''8658''8771'_2576 = erased
-- Data.Rational.Properties.≃⇒≡
d_'8771''8658''8801'_2578 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8771''8658''8801'_2578 = erased
-- Data.Rational.Properties._.1+d₁∣1+d₂
d_1'43'd'8321''8739'1'43'd'8322'_2598 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_1'43'd'8321''8739'1'43'd'8322'_2598 v0 v1 ~v2 v3 v4 ~v5 ~v6
  = du_1'43'd'8321''8739'1'43'd'8322'_2598 v0 v1 v3 v4
du_1'43'd'8321''8739'1'43'd'8322'_2598 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_1'43'd'8321''8739'1'43'd'8322'_2598 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Integer.Coprimality.du_coprime'45'divisor_22
      (coe addInt (coe (1 :: Integer)) (coe v1)) (coe v0)
      (coe addInt (coe (1 :: Integer)) (coe v3))
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
         (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v2)))
-- Data.Rational.Properties._.1+d₂∣1+d₁
d_1'43'd'8322''8739'1'43'd'8321'_2600 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_1'43'd'8322''8739'1'43'd'8321'_2600 v0 v1 ~v2 v3 v4 ~v5 ~v6
  = du_1'43'd'8322''8739'1'43'd'8321'_2600 v0 v1 v3 v4
du_1'43'd'8322''8739'1'43'd'8321'_2600 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_1'43'd'8322''8739'1'43'd'8321'_2600 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Integer.Coprimality.du_coprime'45'divisor_22
      (coe addInt (coe (1 :: Integer)) (coe v3)) (coe v2)
      (coe addInt (coe (1 :: Integer)) (coe v1))
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
         (MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0)))
-- Data.Rational.Properties._.helper
d_helper_2602 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_helper_2602 = erased
-- Data.Rational.Properties.↥p≡0⇒p≡0
d_'8613'p'8801'0'8658'p'8801'0_2614 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613'p'8801'0'8658'p'8801'0_2614 = erased
-- Data.Rational.Properties._.d-1≡0
d_d'45'1'8801'0_2626 ::
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_d'45'1'8801'0_2626 = erased
-- Data.Rational.Properties.p≡0⇒↥p≡0
d_p'8801'0'8658''8613'p'8801'0_2630 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p'8801'0'8658''8613'p'8801'0_2630 = erased
-- Data.Rational.Properties.nonNeg≢neg
d_nonNeg'8802'neg_2638 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_nonNeg'8802'neg_2638 = erased
-- Data.Rational.Properties.pos⇒nonNeg
d_pos'8658'nonNeg_2642 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_pos'8658'nonNeg_2642 v0 ~v1 = du_pos'8658'nonNeg_2642 v0
du_pos'8658'nonNeg_2642 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_pos'8658'nonNeg_2642 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_pos'8658'nonNeg_710
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
-- Data.Rational.Properties.neg⇒nonPos
d_neg'8658'nonPos_2648 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154
d_neg'8658'nonPos_2648 v0 ~v1 = du_neg'8658'nonPos_2648 v0
du_neg'8658'nonPos_2648 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154
du_neg'8658'nonPos_2648 v0
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_neg'8658'nonPos_716
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
-- Data.Rational.Properties.nonNeg∧nonZero⇒pos
d_nonNeg'8743'nonZero'8658'pos_2654 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_nonNeg'8743'nonZero'8658'pos_2654 v0 ~v1 ~v2
  = du_nonNeg'8743'nonZero'8658'pos_2654 v0
du_nonNeg'8743'nonZero'8658'pos_2654 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_nonNeg'8743'nonZero'8658'pos_2654 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_Positive'46'constructor_1295
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Properties.pos⇒nonZero
d_pos'8658'nonZero_2658 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_pos'8658'nonZero_2658 v0 ~v1 = du_pos'8658'nonZero_2658 v0
du_pos'8658'nonZero_2658 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_pos'8658'nonZero_2658 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Properties.neg⇒nonZero
d_neg'8658'nonZero_2662 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_neg'8658'nonZero_2662 v0 ~v1 = du_neg'8658'nonZero_2662 v0
du_neg'8658'nonZero_2662 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_neg'8658'nonZero_2662 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Properties.↥-neg
d_'8613''45'neg_2666 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''45'neg_2666 = erased
-- Data.Rational.Properties.↧-neg
d_'8615''45'neg_2670 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''45'neg_2670 = erased
-- Data.Rational.Properties.neg-injective
d_neg'45'injective_2672 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'injective_2672 = erased
-- Data.Rational.Properties.neg-pos
d_neg'45'pos_2694 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
d_neg'45'pos_2694 v0 ~v1 = du_neg'45'pos_2694 v0
du_neg'45'pos_2694 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
du_neg'45'pos_2694 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_Negative'46'constructor_1469
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Properties.normalize-coprime
d_normalize'45'coprime_2702 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_normalize'45'coprime_2702 = erased
-- Data.Rational.Properties._.d
d_d_2714 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer
d_d_2714 ~v0 v1 ~v2 = du_d_2714 v1
du_d_2714 :: Integer -> Integer
du_d_2714 v0 = coe addInt (coe (1 :: Integer)) (coe v0)
-- Data.Rational.Properties._.g
d_g_2716 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer
d_g_2716 v0 v1 ~v2 = du_g_2716 v0 v1
du_g_2716 :: Integer -> Integer -> Integer
du_g_2716 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0)
      (coe du_d_2714 (coe v1))
-- Data.Rational.Properties._.c′
d_c'8242'_2718 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_c'8242'_2718 = erased
-- Data.Rational.Properties._.c₂
d_c'8322'_2720 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_c'8322'_2720 = erased
-- Data.Rational.Properties._.g≡1
d_g'8801'1_2722 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_g'8801'1_2722 = erased
-- Data.Rational.Properties._.g≢0
d_g'8802'0_2724 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_g'8802'0_2724 v0 v1 ~v2 = du_g'8802'0_2724 v0 v1
du_g'8802'0_2724 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_g'8802'0_2724 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Base.du_'8802''45'nonZero_102
      (coe
         MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0)
         (coe du_d_2714 (coe v1)))
-- Data.Rational.Properties.↥-normalize
d_'8613''45'normalize_2738 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''45'normalize_2738 = erased
-- Data.Rational.Properties._.g
d_g_2748 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_g_2748 v0 v1 ~v2 = du_g_2748 v0 v1
du_g_2748 :: Integer -> Integer -> Integer
du_g_2748 v0 v1
  = coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1)
-- Data.Rational.Properties._.g≢0
d_g'8802'0_2750 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_g'8802'0_2750 v0 v1 ~v2 = du_g'8802'0_2750 v0 v1
du_g'8802'0_2750 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_g'8802'0_2750 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Base.du_'8802''45'nonZero_102
      (coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1))
-- Data.Rational.Properties._.i/g
d_i'47'g_2754 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_i'47'g_2754 v0 v1 ~v2 = du_i'47'g_2754 v0 v1
du_i'47'g_2754 :: Integer -> Integer -> Integer
du_i'47'g_2754 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0)
      (coe du_g_2748 (coe v0) (coe v1))
-- Data.Rational.Properties.↧-normalize
d_'8615''45'normalize_2766 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''45'normalize_2766 = erased
-- Data.Rational.Properties._.g
d_g_2776 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_g_2776 v0 v1 ~v2 = du_g_2776 v0 v1
du_g_2776 :: Integer -> Integer -> Integer
du_g_2776 v0 v1
  = coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1)
-- Data.Rational.Properties.normalize-cong
d_normalize'45'cong_2798 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_normalize'45'cong_2798 = erased
-- Data.Rational.Properties._.g
d_g_2808 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_g_2808 v0 v1 ~v2 ~v3 = du_g_2808 v0 v1
du_g_2808 :: Integer -> Integer -> Integer
du_g_2808 v0 v1
  = coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1)
-- Data.Rational.Properties.normalize-nonNeg
d_normalize'45'nonNeg_2820 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_normalize'45'nonNeg_2820 ~v0 ~v1 ~v2
  = du_normalize'45'nonNeg_2820
du_normalize'45'nonNeg_2820 ::
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
du_normalize'45'nonNeg_2820 = coe du_mkℚ'43''45'nonNeg_2554
-- Data.Rational.Properties._.g
d_g_2830 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_g_2830 v0 v1 ~v2 = du_g_2830 v0 v1
du_g_2830 :: Integer -> Integer -> Integer
du_g_2830 v0 v1
  = coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1)
-- Data.Rational.Properties.normalize-pos
d_normalize'45'pos_2844 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_normalize'45'pos_2844 ~v0 ~v1 ~v2 ~v3 = du_normalize'45'pos_2844
du_normalize'45'pos_2844 ::
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_normalize'45'pos_2844 = coe du_mkℚ'43''45'pos_2570
-- Data.Rational.Properties._.g≢0
d_g'8802'0_2856 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_g'8802'0_2856 v0 v1 ~v2 ~v3 = du_g'8802'0_2856 v0 v1
du_g'8802'0_2856 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_g'8802'0_2856 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Base.du_'8802''45'nonZero_102
      (coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1))
-- Data.Rational.Properties.normalize-injective-≃
d_normalize'45'injective'45''8771'_2874 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_normalize'45'injective'45''8771'_2874 = erased
-- Data.Rational.Properties._.gcd[m,c]
d_gcd'91'm'44'c'93'_2890 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_gcd'91'm'44'c'93'_2890 v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6
  = du_gcd'91'm'44'c'93'_2890 v0 v2
du_gcd'91'm'44'c'93'_2890 :: Integer -> Integer -> Integer
du_gcd'91'm'44'c'93'_2890 v0 v1
  = coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1)
-- Data.Rational.Properties._.gcd[n,d]
d_gcd'91'n'44'd'93'_2892 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_gcd'91'n'44'd'93'_2892 ~v0 v1 ~v2 v3 ~v4 ~v5 ~v6
  = du_gcd'91'n'44'd'93'_2892 v1 v3
du_gcd'91'n'44'd'93'_2892 :: Integer -> Integer -> Integer
du_gcd'91'n'44'd'93'_2892 v0 v1
  = coe MAlonzo.Code.Data.Nat.GCD.d_gcd_148 (coe v0) (coe v1)
-- Data.Rational.Properties._.gcd[m,c]∣m
d_gcd'91'm'44'c'93''8739'm_2894 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'm'44'c'93''8739'm_2894 v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6
  = du_gcd'91'm'44'c'93''8739'm_2894 v0 v2
du_gcd'91'm'44'c'93''8739'm_2894 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_gcd'91'm'44'c'93''8739'm_2894 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.d_gcd'91'm'44'n'93''8739'm_252 (coe v0)
      (coe v1)
-- Data.Rational.Properties._.gcd[m,c]∣c
d_gcd'91'm'44'c'93''8739'c_2896 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'm'44'c'93''8739'c_2896 v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6
  = du_gcd'91'm'44'c'93''8739'c_2896 v0 v2
du_gcd'91'm'44'c'93''8739'c_2896 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_gcd'91'm'44'c'93''8739'c_2896 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.d_gcd'91'm'44'n'93''8739'n_282 (coe v0)
      (coe v1)
-- Data.Rational.Properties._.gcd[n,d]∣n
d_gcd'91'n'44'd'93''8739'n_2898 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'n'44'd'93''8739'n_2898 ~v0 v1 ~v2 v3 ~v4 ~v5 ~v6
  = du_gcd'91'n'44'd'93''8739'n_2898 v1 v3
du_gcd'91'n'44'd'93''8739'n_2898 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_gcd'91'n'44'd'93''8739'n_2898 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.d_gcd'91'm'44'n'93''8739'm_252 (coe v0)
      (coe v1)
-- Data.Rational.Properties._.gcd[n,d]∣d
d_gcd'91'n'44'd'93''8739'd_2900 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'n'44'd'93''8739'd_2900 ~v0 v1 ~v2 v3 ~v4 ~v5 ~v6
  = du_gcd'91'n'44'd'93''8739'd_2900 v1 v3
du_gcd'91'n'44'd'93''8739'd_2900 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_gcd'91'n'44'd'93''8739'd_2900 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.d_gcd'91'm'44'n'93''8739'n_282 (coe v0)
      (coe v1)
-- Data.Rational.Properties._.md∣gcd[m,c]gcd[n,d]
d_md'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2902 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_md'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2902 v0 v1 v2 v3 ~v4
                                                  ~v5 ~v6
  = du_md'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2902 v0 v1 v2 v3
du_md'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2902 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_md'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2902 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Nat.Divisibility.Core.du_'42''45'pres'45''8739'_42
      (coe du_gcd'91'm'44'c'93''8739'm_2894 (coe v0) (coe v2))
      (coe du_gcd'91'n'44'd'93''8739'd_2900 (coe v1) (coe v3))
-- Data.Rational.Properties._.nc∣gcd[n,d]gcd[m,c]
d_nc'8739'gcd'91'n'44'd'93'gcd'91'm'44'c'93'_2904 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_nc'8739'gcd'91'n'44'd'93'gcd'91'm'44'c'93'_2904 v0 v1 v2 v3 ~v4
                                                  ~v5 ~v6
  = du_nc'8739'gcd'91'n'44'd'93'gcd'91'm'44'c'93'_2904 v0 v1 v2 v3
du_nc'8739'gcd'91'n'44'd'93'gcd'91'm'44'c'93'_2904 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_nc'8739'gcd'91'n'44'd'93'gcd'91'm'44'c'93'_2904 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Nat.Divisibility.Core.du_'42''45'pres'45''8739'_42
      (coe du_gcd'91'n'44'd'93''8739'n_2898 (coe v1) (coe v3))
      (coe du_gcd'91'm'44'c'93''8739'c_2896 (coe v0) (coe v2))
-- Data.Rational.Properties._.nc∣gcd[m,c]gcd[n,d]
d_nc'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2906 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_nc'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2906 v0 v1 v2 v3 ~v4
                                                  ~v5 ~v6
  = du_nc'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2906 v0 v1 v2 v3
du_nc'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2906 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_nc'8739'gcd'91'm'44'c'93'gcd'91'n'44'd'93'_2906 v0 v1 v2 v3
  = coe
      du_nc'8739'gcd'91'n'44'd'93'gcd'91'm'44'c'93'_2904 (coe v0)
      (coe v1) (coe v2) (coe v3)
-- Data.Rational.Properties._.gcd[m,c]≢0′
d_gcd'91'm'44'c'93''8802'0'8242'_2910 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_gcd'91'm'44'c'93''8802'0'8242'_2910 = erased
-- Data.Rational.Properties._.gcd[n,d]≢0′
d_gcd'91'n'44'd'93''8802'0'8242'_2912 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_gcd'91'n'44'd'93''8802'0'8242'_2912 = erased
-- Data.Rational.Properties._.gcd[m,c]*gcd[n,d]≢0′
d_gcd'91'm'44'c'93''42'gcd'91'n'44'd'93''8802'0'8242'_2914 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_gcd'91'm'44'c'93''42'gcd'91'n'44'd'93''8802'0'8242'_2914 = erased
-- Data.Rational.Properties._.div
d_div_2930 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_div_2930 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 = du_div_2930
du_div_2930 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_div_2930 = coe du_mkℚ'43''45'injective_2516
-- Data.Rational.Properties._.m/gcd[m,c]≡n/gcd[n,d]
d_m'47'gcd'91'm'44'c'93''8801'n'47'gcd'91'n'44'd'93'_2932 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'47'gcd'91'm'44'c'93''8801'n'47'gcd'91'n'44'd'93'_2932 = erased
-- Data.Rational.Properties._.c/gcd[m,c]≡d/gcd[n,d]
d_c'47'gcd'91'm'44'c'93''8801'd'47'gcd'91'n'44'd'93'_2934 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_c'47'gcd'91'm'44'c'93''8801'd'47'gcd'91'n'44'd'93'_2934 = erased
-- Data.Rational.Properties.↥-/
d_'8613''45''47'_2942 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''45''47'_2942 = erased
-- Data.Rational.Properties._.g
d_g_2956 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_g_2956 v0 v1 ~v2 = du_g_2956 v0 v1
du_g_2956 :: Integer -> Integer -> Integer
du_g_2956 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.d_gcd_148
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Rational.Properties._.norm
d_norm_2958 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_norm_2958 v0 v1 ~v2 = du_norm_2958 v0 v1
du_norm_2958 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_norm_2958 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Rational.Base.du_normalize_128
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Rational.Properties.↧-/
d_'8615''45''47'_2968 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''45''47'_2968 = erased
-- Data.Rational.Properties._.g
d_g_2982 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_g_2982 v0 v1 ~v2 = du_g_2982 v0 v1
du_g_2982 :: Integer -> Integer -> Integer
du_g_2982 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.d_gcd_148
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Rational.Properties._.norm
d_norm_2984 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_norm_2984 v0 v1 ~v2 = du_norm_2984 v0 v1
du_norm_2984 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_norm_2984 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Rational.Base.du_normalize_128
      (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Rational.Properties.↥p/↧p≡p
d_'8613'p'47''8615'p'8801'p_2990 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613'p'47''8615'p'8801'p_2990 = erased
-- Data.Rational.Properties.0/n≡0
d_0'47'n'8801'0_3008 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'47'n'8801'0_3008 = erased
-- Data.Rational.Properties._.0-cop-1
d_0'45'cop'45'1_3020 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'45'cop'45'1_3020 = erased
-- Data.Rational.Properties._.n/n≢0
d_n'47'n'8802'0_3022 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_n'47'n'8802'0_3022 ~v0 ~v1 = du_n'47'n'8802'0_3022
du_n'47'n'8802'0_3022 :: MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_n'47'n'8802'0_3022
  = coe
      MAlonzo.Code.Data.Nat.Base.du_'62''45'nonZero_112
      (coe
         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
         (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
-- Data.Rational.Properties./-cong
d_'47''45'cong_3038 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''45'cong_3038 = erased
-- Data.Rational.Properties./-injective-≃-helper
d_'47''45'injective'45''8771''45'helper_3056 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'47''45'injective'45''8771''45'helper_3056 ~v0 ~v1 ~v2 ~v3 ~v4
                                             ~v5 ~v6
  = du_'47''45'injective'45''8771''45'helper_3056
du_'47''45'injective'45''8771''45'helper_3056 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'47''45'injective'45''8771''45'helper_3056
  = coe
      MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
-- Data.Rational.Properties./-injective-≃
d_'47''45'injective'45''8771'_3080 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_'47''45'injective'45''8771'_3080 v0 v1 ~v2
  = du_'47''45'injective'45''8771'_3080 v0 v1
du_'47''45'injective'45''8771'_3080 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_'47''45'injective'45''8771'_3080 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v2 v3
        -> case coe v2 of
             _ | coe geqInt (coe v2) (coe (0 :: Integer)) ->
                 case coe v1 of
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
                     -> case coe v4 of
                          _ | coe geqInt (coe v4) (coe (0 :: Integer)) ->
                              coe
                                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30
                          _ -> coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'sym_134
                                 (coe du_'47''45'injective'45''8771''45'helper_3056)
                   _ -> MAlonzo.RTE.mazUnreachableError
             _ -> case coe v1 of
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.C_mkℚ'7512'_22 v4 v5
                      -> case coe v4 of
                           _ | coe geqInt (coe v4) (coe (0 :: Integer)) ->
                               coe du_'47''45'injective'45''8771''45'helper_3056
                           _ -> coe
                                  MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30
                    _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.↥ᵘ-toℚᵘ
d_'8613''7512''45'toℚ'7512'_3128 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''7512''45'toℚ'7512'_3128 = erased
-- Data.Rational.Properties.↧ᵘ-toℚᵘ
d_'8615''7512''45'toℚ'7512'_3134 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''7512''45'toℚ'7512'_3134 = erased
-- Data.Rational.Properties.toℚᵘ-injective
d_toℚ'7512''45'injective_3138 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_toℚ'7512''45'injective_3138 = erased
-- Data.Rational.Properties.fromℚᵘ-injective
d_fromℚ'7512''45'injective_3146 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_fromℚ'7512''45'injective_3146 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (\ v2 ->
            coe du_'47''45'injective'45''8771'_3080 (coe v0) (coe v1)))
-- Data.Rational.Properties.fromℚᵘ-toℚᵘ
d_fromℚ'7512''45'toℚ'7512'_3154 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℚ'7512''45'toℚ'7512'_3154 = erased
-- Data.Rational.Properties.toℚᵘ-fromℚᵘ
d_toℚ'7512''45'fromℚ'7512'_3170 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_toℚ'7512''45'fromℚ'7512'_3170 v0
  = coe
      d_fromℚ'7512''45'injective_3146
      (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
         (coe MAlonzo.Code.Data.Rational.Base.d_fromℚ'7512'_164 (coe v0)))
      v0 erased
-- Data.Rational.Properties.toℚᵘ-cong
d_toℚ'7512''45'cong_3174 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_toℚ'7512''45'cong_3174 ~v0 ~v1 ~v2 = du_toℚ'7512''45'cong_3174
du_toℚ'7512''45'cong_3174 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_toℚ'7512''45'cong_3174
  = coe
      MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30
-- Data.Rational.Properties.fromℚᵘ-cong
d_fromℚ'7512''45'cong_3176 ::
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromℚ'7512''45'cong_3176 = erased
-- Data.Rational.Properties.toℚᵘ-isRelHomomorphism
d_toℚ'7512''45'isRelHomomorphism_3188 ::
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_toℚ'7512''45'isRelHomomorphism_3188
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsRelHomomorphism'46'constructor_587
      (\ v0 v1 v2 -> coe du_toℚ'7512''45'cong_3174)
-- Data.Rational.Properties.toℚᵘ-isRelMonomorphism
d_toℚ'7512''45'isRelMonomorphism_3190 ::
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_toℚ'7512''45'isRelMonomorphism_3190
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsRelMonomorphism'46'constructor_1555
      (coe d_toℚ'7512''45'isRelHomomorphism_3188) erased
-- Data.Rational.Properties.drop-*≤*
d_drop'45''42''8804''42'_3192 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
d_drop'45''42''8804''42'_3192 ~v0 ~v1 v2
  = du_drop'45''42''8804''42'_3192 v2
du_drop'45''42''8804''42'_3192 ::
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Integer.Base.T__'8804'__26
du_drop'45''42''8804''42'_3192 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.toℚᵘ-mono-≤
d_toℚ'7512''45'mono'45''8804'_3196 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38
d_toℚ'7512''45'mono'45''8804'_3196 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (case coe v2 of
            MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v5
              -> coe
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v5
            _ -> MAlonzo.RTE.mazUnreachableError))
-- Data.Rational.Properties.toℚᵘ-cancel-≤
d_toℚ'7512''45'cancel'45''8804'_3204 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_toℚ'7512''45'cancel'45''8804'_3204 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (case coe v2 of
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8804''42'_44 v5
              -> coe MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v5
            _ -> MAlonzo.RTE.mazUnreachableError))
-- Data.Rational.Properties.toℚᵘ-isOrderHomomorphism-≤
d_toℚ'7512''45'isOrderHomomorphism'45''8804'_3212 ::
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
d_toℚ'7512''45'isOrderHomomorphism'45''8804'_3212
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsOrderHomomorphism'46'constructor_5407
      (\ v0 v1 v2 -> coe du_toℚ'7512''45'cong_3174)
      (coe d_toℚ'7512''45'mono'45''8804'_3196)
-- Data.Rational.Properties.toℚᵘ-isOrderMonomorphism-≤
d_toℚ'7512''45'isOrderMonomorphism'45''8804'_3214 ::
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderMonomorphism_182
d_toℚ'7512''45'isOrderMonomorphism'45''8804'_3214
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsOrderMonomorphism'46'constructor_9041
      (coe d_toℚ'7512''45'isOrderHomomorphism'45''8804'_3212) erased
      (coe d_toℚ'7512''45'cancel'45''8804'_3204)
-- Data.Rational.Properties.≤-Monomorphism.trans
d_trans_3258 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8804'__38) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_trans_3258
  = let v0 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v1 = d_toℚ'7512''45'isOrderMonomorphism'45''8804'_3214 in
    coe
      MAlonzo.Code.Relation.Binary.Morphism.RelMonomorphism.du_trans_46
      (coe v0)
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.du_isRelMonomorphism_218
         (coe v1))
-- Data.Rational.Properties.≤-Monomorphism.EqM.trans
d_trans_3276 ::
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_3276 = erased
-- Data.Rational.Properties.≤-reflexive
d_'8804''45'reflexive_3278 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8804''45'reflexive_3278 v0 ~v1 ~v2
  = du_'8804''45'reflexive_3278 v0
du_'8804''45'reflexive_3278 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'8804''45'reflexive_3278 v0
  = coe
      MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
      (MAlonzo.Code.Data.Integer.Properties.d_'8804''45'refl_2512
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))))
-- Data.Rational.Properties.≤-refl
d_'8804''45'refl_3280 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8804''45'refl_3280 v0 = coe du_'8804''45'reflexive_3278 (coe v0)
-- Data.Rational.Properties.≤-trans
d_'8804''45'trans_3282 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8804''45'trans_3282 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.RelMonomorphism.du_trans_46
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.du_isRelMonomorphism_218
         (coe d_toℚ'7512''45'isOrderMonomorphism'45''8804'_3214))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'trans_250)
      (coe v0) (coe v1) (coe v2)
-- Data.Rational.Properties.≤-antisym
d_'8804''45'antisym_3284 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'antisym_3284 = erased
-- Data.Rational.Properties.≤-total
d_'8804''45'total_3290 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8804''45'total_3290 v0 v1
  = coe
      MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
      (\ v2 ->
         coe
           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
           (coe MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v2))
      (\ v2 ->
         coe
           MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
           (coe MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v2))
      (MAlonzo.Code.Data.Integer.Properties.d_'8804''45'total_2538
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))))
-- Data.Rational.Properties._≤?_
d__'8804''63'__3296 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__3296 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52)
      (coe
         MAlonzo.Code.Data.Integer.Properties.d__'8804''63'__2556
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))))
-- Data.Rational.Properties._≥?_
d__'8805''63'__3302 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8805''63'__3302 v0 v1
  = coe d__'8804''63'__3296 (coe v1) (coe v0)
-- Data.Rational.Properties.≤-irrelevant
d_'8804''45'irrelevant_3304 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'irrelevant_3304 = erased
-- Data.Rational.Properties.≤-isPreorder
d_'8804''45'isPreorder_3310 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder_3310
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'8804''45'reflexive_3278 v0)
      (coe d_'8804''45'trans_3282)
-- Data.Rational.Properties.≤-isTotalPreorder
d_'8804''45'isTotalPreorder_3312 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_'8804''45'isTotalPreorder_3312
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalPreorder'46'constructor_7157
      (coe d_'8804''45'isPreorder_3310) (coe d_'8804''45'total_3290)
-- Data.Rational.Properties.≤-isPartialOrder
d_'8804''45'isPartialOrder_3314 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8804''45'isPartialOrder_3314
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe d_'8804''45'isPreorder_3310) erased
-- Data.Rational.Properties.≤-isTotalOrder
d_'8804''45'isTotalOrder_3316 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8804''45'isTotalOrder_3316
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe d_'8804''45'isPartialOrder_3314) (coe d_'8804''45'total_3290)
-- Data.Rational.Properties.≤-isDecTotalOrder
d_'8804''45'isDecTotalOrder_3318 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_'8804''45'isDecTotalOrder_3318
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe d_'8804''45'isTotalOrder_3316) (coe d__'8799'__2468)
      (coe d__'8804''63'__3296)
-- Data.Rational.Properties.≤-totalPreorder
d_'8804''45'totalPreorder_3320 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
d_'8804''45'totalPreorder_3320
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalPreorder'46'constructor_3645
      d_'8804''45'isTotalPreorder_3312
-- Data.Rational.Properties.≤-decTotalOrder
d_'8804''45'decTotalOrder_3322 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_'8804''45'decTotalOrder_3322
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      d_'8804''45'isDecTotalOrder_3318
-- Data.Rational.Properties.drop-*<*
d_drop'45''42''60''42'_3324 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
d_drop'45''42''60''42'_3324 ~v0 ~v1 v2
  = du_drop'45''42''60''42'_3324 v2
du_drop'45''42''60''42'_3324 ::
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Integer.Base.T__'60'__50
du_drop'45''42''60''42'_3324 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.toℚᵘ-mono-<
d_toℚ'7512''45'mono'45''60'_3328 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46
d_toℚ'7512''45'mono'45''60'_3328 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (case coe v2 of
            MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v5
              -> coe
                   MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v5
            _ -> MAlonzo.RTE.mazUnreachableError))
-- Data.Rational.Properties.toℚᵘ-cancel-<
d_toℚ'7512''45'cancel'45''60'_3336 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'60'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_toℚ'7512''45'cancel'45''60'_3336 v0 v1 v2
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (case coe v2 of
            MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''60''42'_52 v5
              -> coe MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v5
            _ -> MAlonzo.RTE.mazUnreachableError))
-- Data.Rational.Properties.toℚᵘ-isOrderHomomorphism-<
d_toℚ'7512''45'isOrderHomomorphism'45''60'_3344 ::
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
d_toℚ'7512''45'isOrderHomomorphism'45''60'_3344
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsOrderHomomorphism'46'constructor_5407
      (\ v0 v1 v2 -> coe du_toℚ'7512''45'cong_3174)
      (coe d_toℚ'7512''45'mono'45''60'_3328)
-- Data.Rational.Properties.toℚᵘ-isOrderMonomorphism-<
d_toℚ'7512''45'isOrderMonomorphism'45''60'_3346 ::
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderMonomorphism_182
d_toℚ'7512''45'isOrderMonomorphism'45''60'_3346
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsOrderMonomorphism'46'constructor_9041
      (coe d_toℚ'7512''45'isOrderHomomorphism'45''60'_3344) erased
      (coe d_toℚ'7512''45'cancel'45''60'_3336)
-- Data.Rational.Properties.<⇒≤
d_'60''8658''8804'_3348 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'60''8658''8804'_3348 ~v0 ~v1 v2 = du_'60''8658''8804'_3348 v2
du_'60''8658''8804'_3348 ::
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'60''8658''8804'_3348 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v3
        -> coe
             MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
             (coe
                MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630
                (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.≮⇒≥
d_'8814''8658''8805'_3352 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8814''8658''8805'_3352 v0 v1 ~v2
  = du_'8814''8658''8805'_3352 v0 v1
du_'8814''8658''8805'_3352 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'8814''8658''8805'_3352 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8814''8658''8805'_2684
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))))
-- Data.Rational.Properties.≰⇒>
d_'8816''8658''62'_3360 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  (MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'8816''8658''62'_3360 v0 v1 ~v2 = du_'8816''8658''62'_3360 v0 v1
du_'8816''8658''62'_3360 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'8816''8658''62'_3360 v0 v1
  = coe
      MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8816''8658''62'_2658
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))))
-- Data.Rational.Properties.<⇒≢
d_'60''8658''8802'_3368 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8802'_3368 = erased
-- Data.Rational.Properties.<-irrefl
d_'60''45'irrefl_3376 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl_3376 = erased
-- Data.Rational.Properties.<-asym
d_'60''45'asym_3380 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'asym_3380 = erased
-- Data.Rational.Properties.<-≤-trans
d_'60''45''8804''45'trans_3386 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'60''45''8804''45'trans_3386 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v7
        -> case coe v4 of
             MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v10
               -> coe
                    MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                    (coe
                       MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''60''45'nonNeg_6064
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
                          (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                          (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                       (addInt
                          (coe (1 :: Integer))
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d_denominator'45'1_16 (coe v1)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                             (\ v11 v12 v13 v14 v15 ->
                                coe
                                  MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v14
                                  v15)
                             (coe
                                MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                             (\ v11 v12 v13 v14 v15 ->
                                coe
                                  MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                  v14 v15)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                (\ v11 v12 v13 v14 v15 ->
                                   coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                     v14 v15)
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                         (coe v2))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                         (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                   (coe
                                      MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                            (coe v0)))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                         (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'737''45''8804''45'nonNeg_5856
                                   (MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
                                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                                   (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                   v10))
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''60''45'pos_6006
                                (MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2))
                                (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                v7))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.≤-<-trans
d_'8804''45''60''45'trans_3420 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'8804''45''60''45'trans_3420 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v7
        -> case coe v4 of
             MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v10
               -> coe
                    MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                    (coe
                       MAlonzo.Code.Data.Integer.Properties.du_'42''45'cancel'691''45''60''45'nonNeg_6064
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
                          (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                       (MAlonzo.Code.Data.Integer.Base.d__'42'__308
                          (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                          (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                       (addInt
                          (coe (1 :: Integer))
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d_denominator'45'1_16 (coe v1)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                             (\ v11 v12 v13 v14 v15 ->
                                coe
                                  MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                                  v14 v15)
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                                (\ v11 v12 v13 v14 v15 ->
                                   coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'60''45'trans_2770 v14
                                     v15)
                                (coe
                                   MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                                (\ v11 v12 v13 v14 v15 ->
                                   coe
                                     MAlonzo.Code.Data.Integer.Properties.du_'60''45''8804''45'trans_2756
                                     v14 v15)
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                         (coe v2))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                         (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                                   (coe
                                      MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe
                                         MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                            (coe v0)))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22
                                         (coe v1))))
                                (coe
                                   MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'737''45''60''45'pos_5974
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2)))
                                   (coe
                                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                      (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v2))
                                      (coe
                                         MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                   (coe v10)))
                             (coe
                                MAlonzo.Code.Data.Integer.Properties.du_'42''45'mono'691''45''8804''45'nonNeg_5814
                                (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v2))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
                                (coe
                                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                                   (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                                   (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
                                (coe v7)))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.<-trans
d_'60''45'trans_3454 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'60''45'trans_3454 v0 v1 v2 v3
  = coe
      d_'8804''45''60''45'trans_3420 (coe v0) (coe v1) (coe v2)
      (coe du_'60''8658''8804'_3348 (coe v3))
-- Data.Rational.Properties._<?_
d__'60''63'__3458 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__3458 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60)
      (coe
         MAlonzo.Code.Data.Integer.Properties.d__'60''63'__2866
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
         (coe
            MAlonzo.Code.Data.Integer.Base.d__'42'__308
            (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
            (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))))
-- Data.Rational.Properties._>?_
d__'62''63'__3464 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'62''63'__3464 v0 v1 = coe d__'60''63'__3458 (coe v1) (coe v0)
-- Data.Rational.Properties.<-cmp
d_'60''45'cmp_3466 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_'60''45'cmp_3466 v0 v1
  = let v2
          = MAlonzo.Code.Data.Integer.Properties.d_'60''45'cmp_2776
              (coe
                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                 (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
                 (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
              (coe
                 MAlonzo.Code.Data.Integer.Base.d__'42'__308
                 (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
                 (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))) in
    case coe v2 of
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v3
        -> coe
             MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150
             (coe MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v3)
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v4
        -> coe
             MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 erased
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v5
        -> coe
             MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166
             (coe MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.<-irrelevant
d_'60''45'irrelevant_3506 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'60''45'irrelevant_3506 = erased
-- Data.Rational.Properties.<-respʳ-≡
d_'60''45'resp'691''45''8801'_3512 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'60''45'resp'691''45''8801'_3512 ~v0 ~v1 ~v2 ~v3 v4
  = du_'60''45'resp'691''45''8801'_3512 v4
du_'60''45'resp'691''45''8801'_3512 ::
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'60''45'resp'691''45''8801'_3512 v0 = coe v0
-- Data.Rational.Properties.<-respˡ-≡
d_'60''45'resp'737''45''8801'_3516 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'60''45'resp'737''45''8801'_3516 ~v0 ~v1 ~v2 ~v3 v4
  = du_'60''45'resp'737''45''8801'_3516 v4
du_'60''45'resp'737''45''8801'_3516 ::
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'60''45'resp'737''45''8801'_3516 v0 = coe v0
-- Data.Rational.Properties.<-resp-≡
d_'60''45'resp'45''8801'_3520 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8801'_3520
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4))
-- Data.Rational.Properties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_3522 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_3522
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      d_'60''45'trans_3454 d_'60''45'resp'45''8801'_3520
-- Data.Rational.Properties.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_3524 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_3524
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (coe d_'60''45'trans_3454) (coe d_'60''45'cmp_3466)
-- Data.Rational.Properties.<-strictPartialOrder
d_'60''45'strictPartialOrder_3526 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_3526
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      d_'60''45'isStrictPartialOrder_3522
-- Data.Rational.Properties.<-strictTotalOrder
d_'60''45'strictTotalOrder_3528 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_3528
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      d_'60''45'isStrictTotalOrder_3524
-- Data.Rational.Properties.≤-Reasoning.Triple._IsRelatedTo_
d__IsRelatedTo__3534 a0 a1 = ()
-- Data.Rational.Properties.≤-Reasoning.Triple._∎
d__'8718'_3536 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8718'_3536
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
      (coe d_'8804''45'isPreorder_3310)
-- Data.Rational.Properties.≤-Reasoning.Triple._≡⟨⟩_
d__'8801''10216''10217'__3538 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d__'8801''10216''10217'__3538 v0 = coe v0
-- Data.Rational.Properties.≤-Reasoning.Triple.IsEquality
d_IsEquality_3540 a0 a1 a2 = ()
-- Data.Rational.Properties.≤-Reasoning.Triple.IsEquality?
d_IsEquality'63'_3542 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsEquality'63'_3542 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsEquality'63'_142
      v2
-- Data.Rational.Properties.≤-Reasoning.Triple.IsStrict
d_IsStrict_3544 a0 a1 a2 = ()
-- Data.Rational.Properties.≤-Reasoning.Triple.IsStrict?
d_IsStrict'63'_3546 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsStrict'63'_3546 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_IsStrict'63'_108
      v2
-- Data.Rational.Properties.≤-Reasoning.Triple.begin_
d_begin__3548 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_begin__3548
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe d_'8804''45'isPreorder_3310)
      (\ v0 v1 v2 -> coe du_'60''8658''8804'_3348 v2)
-- Data.Rational.Properties.≤-Reasoning.Triple.begin-equality_
d_begin'45'equality__3550 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_begin'45'equality__3550 = erased
-- Data.Rational.Properties.≤-Reasoning.Triple.begin-strict_
d_begin'45'strict__3552 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  AgdaAny -> MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_begin'45'strict__3552 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      v2
-- Data.Rational.Properties.≤-Reasoning.Triple.extractEquality
d_extractEquality_3556 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsEquality_126 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_extractEquality_3556 = erased
-- Data.Rational.Properties.≤-Reasoning.Triple.extractStrict
d_extractStrict_3558 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T_IsStrict_92 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_extractStrict_3558 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_extractStrict_118
      v2 v3
-- Data.Rational.Properties.≤-Reasoning.Triple.step-<
d_step'45''60'_3566 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''60'_3566
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
      (coe d_'60''45'trans_3454)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
      (coe d_'60''45''8804''45'trans_3386)
-- Data.Rational.Properties.≤-Reasoning.Triple.step-≈
d_step'45''8776'_3568 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8776'_3568
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776'_254
      (coe d_'8804''45'isPreorder_3310)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
-- Data.Rational.Properties.≤-Reasoning.Triple.step-≈˘
d_step'45''8776''728'_3570 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8776''728'_3570
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776''728'_280
      (coe d_'8804''45'isPreorder_3310)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
-- Data.Rational.Properties.≤-Reasoning.Triple.step-≡
d_step'45''8801'_3572 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801'_3572 ~v0 ~v1 ~v2 v3 ~v4
  = du_step'45''8801'_3572 v3
du_step'45''8801'_3572 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801'_3572 v0 = coe v0
-- Data.Rational.Properties.≤-Reasoning.Triple.step-≡˘
d_step'45''8801''728'_3574 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8801''728'_3574 ~v0 ~v1 ~v2 v3 ~v4
  = du_step'45''8801''728'_3574 v3
du_step'45''8801''728'_3574 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
du_step'45''8801''728'_3574 v0 = coe v0
-- Data.Rational.Properties.≤-Reasoning.Triple.step-≤
d_step'45''8804'_3576 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8804'_3576
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
      (coe d_'8804''45'isPreorder_3310)
      (coe d_'8804''45''60''45'trans_3420)
-- Data.Rational.Properties.≤-Reasoning.step-≃
d_step'45''8771'_3596 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8771'_3596
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776'_254
      (coe d_'8804''45'isPreorder_3310)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
-- Data.Rational.Properties.≤-Reasoning.step-≃˘
d_step'45''8771''728'_3598 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T__IsRelatedTo__70
d_step'45''8771''728'_3598
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8776''728'_280
      (coe d_'8804''45'isPreorder_3310)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
-- Data.Rational.Properties.positive⁻¹
d_positive'8315''185'_3602 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_positive'8315''185'_3602 v0 ~v1 = du_positive'8315''185'_3602 v0
du_positive'8315''185'_3602 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_positive'8315''185'_3602 v0
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336
      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170) (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_positive'8315''185'_686
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
-- Data.Rational.Properties.nonNegative⁻¹
d_nonNegative'8315''185'_3608 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_nonNegative'8315''185'_3608 v0 ~v1
  = du_nonNegative'8315''185'_3608 v0
du_nonNegative'8315''185'_3608 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_nonNegative'8315''185'_3608 v0
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204
      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170) (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_nonNegative'8315''185'_692
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
-- Data.Rational.Properties.negative⁻¹
d_negative'8315''185'_3614 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_negative'8315''185'_3614 v0 ~v1 = du_negative'8315''185'_3614 v0
du_negative'8315''185'_3614 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_negative'8315''185'_3614 v0
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336 (coe v0)
      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_negative'8315''185'_698
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
-- Data.Rational.Properties.nonPositive⁻¹
d_nonPositive'8315''185'_3620 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_nonPositive'8315''185'_3620 v0 ~v1
  = du_nonPositive'8315''185'_3620 v0
du_nonPositive'8315''185'_3620 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_nonPositive'8315''185'_3620 v0
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204 (coe v0)
      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_nonPositive'8315''185'_704
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
-- Data.Rational.Properties.neg<pos
d_neg'60'pos_3628 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_neg'60'pos_3628 v0 v1 ~v2 ~v3 = du_neg'60'pos_3628 v0 v1
du_neg'60'pos_3628 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_neg'60'pos_3628 v0 v1
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_neg'60'pos_724
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
-- Data.Rational.Properties.neg-antimono-<
d_neg'45'antimono'45''60'_3634 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_neg'45'antimono'45''60'_3634 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             0 -> case coe v1 of
                    MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v6 v7
                      -> case coe v2 of
                           MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v11
                             -> case coe v6 of
                                  0 -> case coe v11 of
                                         MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v14
                                           -> coe
                                                MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                                                (coe
                                                   MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                                                   v14)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> coe
                                         seq (coe v11)
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                                            (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   seq (coe v1)
                   (case coe v2 of
                      MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v8
                        -> case coe v8 of
                             MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72 v11
                               -> case coe v11 of
                                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v14
                                      -> coe
                                           MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v14)
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError
                      _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> case coe v1 of
                    MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v6 v7
                      -> case coe v6 of
                           0 -> case coe v2 of
                                  MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v11
                                    -> coe
                                         seq (coe v11)
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                                            (coe
                                               MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                                               (coe
                                                  MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                                                  (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ | coe geqInt (coe v6) (coe (1 :: Integer)) ->
                               case coe v2 of
                                 MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v11
                                   -> coe
                                        seq (coe v11)
                                        (coe
                                           MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                                           (coe MAlonzo.Code.Data.Integer.Base.C_'45''60''43'_64))
                                 _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> case coe v2 of
                                  MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60 v11
                                    -> case coe v11 of
                                         MAlonzo.Code.Data.Integer.Base.C_'45''60''45'_58 v14
                                           -> coe
                                                MAlonzo.Code.Data.Rational.Base.C_'42''60''42'_60
                                                (coe
                                                   MAlonzo.Code.Data.Integer.Base.C_'43''60''43'_72
                                                   (coe
                                                      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v14))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.neg-antimono-≤
d_neg'45'antimono'45''8804'_3644 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_neg'45'antimono'45''8804'_3644 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             0 -> case coe v1 of
                    MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v6 v7
                      -> case coe v2 of
                           MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v11
                             -> case coe v6 of
                                  0 -> case coe v11 of
                                         MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v14
                                           -> coe
                                                MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
                                                (coe
                                                   MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                                                   v14)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> coe
                                         seq (coe v11)
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
                                            (coe
                                               MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   seq (coe v1)
                   (case coe v2 of
                      MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v8
                        -> case coe v8 of
                             MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48 v11
                               -> case coe v11 of
                                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v14
                                      -> coe
                                           MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
                                           (coe
                                              MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34
                                              v14)
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError
                      _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> case coe v1 of
                    MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v6 v7
                      -> case coe v6 of
                           0 -> case coe v2 of
                                  MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v11
                                    -> coe
                                         seq (coe v11)
                                         (coe
                                            MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
                                            (coe
                                               MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                                               (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ | coe geqInt (coe v6) (coe (1 :: Integer)) ->
                               case coe v2 of
                                 MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v11
                                   -> coe
                                        seq (coe v11)
                                        (coe
                                           MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
                                           (coe MAlonzo.Code.Data.Integer.Base.C_'45''8804''43'_40))
                                 _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> case coe v2 of
                                  MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52 v11
                                    -> case coe v11 of
                                         MAlonzo.Code.Data.Integer.Base.C_'45''8804''45'_34 v14
                                           -> coe
                                                MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
                                                (coe
                                                   MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                                                   (coe
                                                      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v14))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.≤ᵇ⇒≤
d_'8804''7495''8658''8804'_3654 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  AgdaAny -> MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8804''7495''8658''8804'_3654 v0 v1
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52)
      (\ v2 ->
         coe
           MAlonzo.Code.Data.Integer.Properties.du_'8804''7495''8658''8804'_2604
           (coe
              MAlonzo.Code.Data.Integer.Base.d__'42'__308
              (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
              (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
           (coe
              MAlonzo.Code.Data.Integer.Base.d__'42'__308
              (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
              (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))))
-- Data.Rational.Properties.≤⇒≤ᵇ
d_'8804''8658''8804''7495'_3656 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 -> AgdaAny
d_'8804''8658''8804''7495'_3656 ~v0 ~v1
  = du_'8804''8658''8804''7495'_3656
du_'8804''8658''8804''7495'_3656 ::
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 -> AgdaAny
du_'8804''8658''8804''7495'_3656
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe
         MAlonzo.Code.Data.Integer.Properties.du_'8804''8658''8804''7495'_2612)
      (coe du_drop'45''42''8804''42'_3192)
-- Data.Rational.Properties.↥+ᵘ
d_'8613''43''7512'_3658 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 -> Integer
d_'8613''43''7512'_3658 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'43'__276
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1))
         (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0)))
-- Data.Rational.Properties.↧+ᵘ
d_'8615''43''7512'_3664 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 -> Integer
d_'8615''43''7512'_3664 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'42'__308
      (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
      (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1))
-- Data.Rational.Properties.+-nf
d_'43''45'nf_3670 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 -> Integer
d_'43''45'nf_3670 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.GCD.d_gcd_132
      (coe d_'8613''43''7512'_3658 (coe v0) (coe v1))
      (coe d_'8615''43''7512'_3664 (coe v0) (coe v1))
-- Data.Rational.Properties.↥-+
d_'8613''45''43'_3680 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''45''43'_3680 = erased
-- Data.Rational.Properties.↧-+
d_'8615''45''43'_3690 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''45''43'_3690 = erased
-- Data.Rational.Properties._.Homomorphic₁
d_Homomorphic'8321'_3700 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  ()
d_Homomorphic'8321'_3700 = erased
-- Data.Rational.Properties._.Homomorphic₂
d_Homomorphic'8322'_3702 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8) ->
  ()
d_Homomorphic'8322'_3702 = erased
-- Data.Rational.Properties.toℚᵘ-homo-+
d_toℚ'7512''45'homo'45''43'_3706 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_toℚ'7512''45'homo'45''43'_3706 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6
               -> let v8
                        = MAlonzo.Code.Data.Integer.Properties.d__'8799'__2476
                            (coe
                               d_'43''45'nf_3670
                               (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3)
                               (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6))
                            (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12) in
                  case coe v8 of
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                      -> coe
                           seq (coe v9)
                           (coe
                              seq (coe v10)
                              (coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties._.eq2
d_eq2_3726 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_eq2_3726 = erased
-- Data.Rational.Properties._.eq
d_eq_3728 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_eq_3728 = erased
-- Data.Rational.Properties.toℚᵘ-isMagmaHomomorphism-+
d_toℚ'7512''45'isMagmaHomomorphism'45''43'_3824 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_toℚ'7512''45'isMagmaHomomorphism'45''43'_3824
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaHomomorphism'46'constructor_1049
      (coe d_toℚ'7512''45'isRelHomomorphism_3188)
      (coe d_toℚ'7512''45'homo'45''43'_3706)
-- Data.Rational.Properties.toℚᵘ-isMonoidHomomorphism-+
d_toℚ'7512''45'isMonoidHomomorphism'45''43'_3826 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidHomomorphism_266
d_toℚ'7512''45'isMonoidHomomorphism'45''43'_3826
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMonoidHomomorphism'46'constructor_5533
      (coe d_toℚ'7512''45'isMagmaHomomorphism'45''43'_3824)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'refl_130)
-- Data.Rational.Properties.toℚᵘ-isMonoidMonomorphism-+
d_toℚ'7512''45'isMonoidMonomorphism'45''43'_3828 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288
d_toℚ'7512''45'isMonoidMonomorphism'45''43'_3828
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMonoidMonomorphism'46'constructor_6057
      (coe d_toℚ'7512''45'isMonoidHomomorphism'45''43'_3826) erased
-- Data.Rational.Properties.toℚᵘ-homo‿-
d_toℚ'7512''45'homo'8255''45'_3830 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_toℚ'7512''45'homo'8255''45'_3830 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.toℚᵘ-isGroupHomomorphism-+
d_toℚ'7512''45'isGroupHomomorphism'45''43'_3832 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupHomomorphism_554
d_toℚ'7512''45'isGroupHomomorphism'45''43'_3832
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsGroupHomomorphism'46'constructor_10405
      (coe d_toℚ'7512''45'isMonoidHomomorphism'45''43'_3826)
      (coe d_toℚ'7512''45'homo'8255''45'_3830)
-- Data.Rational.Properties.toℚᵘ-isGroupMonomorphism-+
d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580
d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsGroupMonomorphism'46'constructor_11055
      (coe d_toℚ'7512''45'isGroupHomomorphism'45''43'_3832) erased
-- Data.Rational.Properties.+-Monomorphism.assoc
d_assoc_3838 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_3838 = erased
-- Data.Rational.Properties.+-Monomorphism.comm
d_comm_3846 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_3846 = erased
-- Data.Rational.Properties.+-Monomorphism.identityʳ
d_identity'691'_3854 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identity'691'_3854 = erased
-- Data.Rational.Properties.+-Monomorphism.identityˡ
d_identity'737'_3856 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identity'737'_3856 = erased
-- Data.Rational.Properties.+-Monomorphism.inverse
d_inverse_3858 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_3858
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_inverse_190
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834)
-- Data.Rational.Properties.+-Monomorphism.inverseʳ
d_inverse'691'_3860 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inverse'691'_3860 = erased
-- Data.Rational.Properties.+-Monomorphism.inverseˡ
d_inverse'737'_3862 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inverse'737'_3862 = erased
-- Data.Rational.Properties.+-Monomorphism.isAbelianGroup
d_isAbelianGroup_3864 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_isAbelianGroup_3864
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isAbelianGroup_382
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834)
-- Data.Rational.Properties.+-Monomorphism.isCommutativeMonoid
d_isCommutativeMonoid_3868 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_3868
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834 in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Data.Rational.Properties.+-Monomorphism.isGroup
d_isGroup_3870 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_3870
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isGroup_318
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834)
-- Data.Rational.Properties.+-Monomorphism.isMagma
d_isMagma_3872 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3872
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834 in
    let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Data.Rational.Properties.+-Monomorphism.isMonoid
d_isMonoid_3874 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_3874
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834 in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Data.Rational.Properties.+-Monomorphism.isSemigroup
d_isSemigroup_3878 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3878
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834 in
    let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Data.Rational.Properties.+-Monomorphism.⁻¹-cong
d_'8315''185''45'cong_3888 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_3888 = erased
-- Data.Rational.Properties.+-Monomorphism.⁻¹-distrib-∙
d_'8315''185''45'distrib'45''8729'_3890 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'distrib'45''8729'_3890 = erased
-- Data.Rational.Properties.+-assoc
d_'43''45'assoc_3892 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'assoc_3892 = erased
-- Data.Rational.Properties.+-comm
d_'43''45'comm_3894 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'comm_3894 = erased
-- Data.Rational.Properties.+-identityˡ
d_'43''45'identity'737'_3896 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'737'_3896 = erased
-- Data.Rational.Properties.+-identityʳ
d_'43''45'identity'691'_3898 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'identity'691'_3898 = erased
-- Data.Rational.Properties.+-identity
d_'43''45'identity_3900 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'identity_3900
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Properties.+-inverseˡ
d_'43''45'inverse'737'_3902 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'inverse'737'_3902 = erased
-- Data.Rational.Properties.+-inverseʳ
d_'43''45'inverse'691'_3904 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'inverse'691'_3904 = erased
-- Data.Rational.Properties.+-inverse
d_'43''45'inverse_3906 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_3906
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_inverse_190
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'isMagma_1430)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'inverse_974)
-- Data.Rational.Properties.-‿cong
d_'45''8255'cong_3908 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'45''8255'cong_3908 = erased
-- Data.Rational.Properties.neg-distrib-+
d_neg'45'distrib'45''43'_3914 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'45''43'_3914 = erased
-- Data.Rational.Properties.+-isMagma
d_'43''45'isMagma_3916 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'43''45'isMagma_3916
  = let v0
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
              (coe
                 MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376) in
    let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
              (coe
                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306) in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'isMagma_1430)
-- Data.Rational.Properties.+-isSemigroup
d_'43''45'isSemigroup_3918 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'43''45'isSemigroup_3918
  = let v0
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
              (coe
                 MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376) in
    let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
              (coe
                 MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306) in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'isSemigroup_1432)
-- Data.Rational.Properties.+-0-isMonoid
d_'43''45'0'45'isMonoid_3920 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'0'45'isMonoid_3920
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
         (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306))
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'0'45'isMonoid_1434)
-- Data.Rational.Properties.+-0-isCommutativeMonoid
d_'43''45'0'45'isCommutativeMonoid_3922 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'0'45'isCommutativeMonoid_3922
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
         (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306))
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'0'45'isCommutativeMonoid_1436)
-- Data.Rational.Properties.+-0-isGroup
d_'43''45'0'45'isGroup_3924 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_'43''45'0'45'isGroup_3924
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isGroup_318
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'0'45'isGroup_1438)
-- Data.Rational.Properties.+-0-isAbelianGroup
d_'43''45'0'45'isAbelianGroup_3926 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'0'45'isAbelianGroup_3926
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isAbelianGroup_382
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45'0'45'rawGroup_376)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45'0'45'rawGroup_306)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isGroupMonomorphism'45''43'_3834)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'0'45'isAbelianGroup_1440)
-- Data.Rational.Properties.+-magma
d_'43''45'magma_3928 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'43''45'magma_3928
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Rational.Base.d__'43'__260 d_'43''45'isMagma_3916
-- Data.Rational.Properties.+-semigroup
d_'43''45'semigroup_3930 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'43''45'semigroup_3930
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      d_'43''45'isSemigroup_3918
-- Data.Rational.Properties.+-0-monoid
d_'43''45'0'45'monoid_3932 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'43''45'0'45'monoid_3932
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
      d_'43''45'0'45'isMonoid_3920
-- Data.Rational.Properties.+-0-commutativeMonoid
d_'43''45'0'45'commutativeMonoid_3934 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'0'45'commutativeMonoid_3934
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
      d_'43''45'0'45'isCommutativeMonoid_3922
-- Data.Rational.Properties.+-0-group
d_'43''45'0'45'group_3936 ::
  MAlonzo.Code.Algebra.Bundles.T_Group_1266
d_'43''45'0'45'group_3936
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Group'46'constructor_21965
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
      MAlonzo.Code.Data.Rational.Base.d_'45'__104
      d_'43''45'0'45'isGroup_3924
-- Data.Rational.Properties.+-0-abelianGroup
d_'43''45'0'45'abelianGroup_3938 ::
  MAlonzo.Code.Algebra.Bundles.T_AbelianGroup_1378
d_'43''45'0'45'abelianGroup_3938
  = coe
      MAlonzo.Code.Algebra.Bundles.C_AbelianGroup'46'constructor_24425
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
      MAlonzo.Code.Data.Rational.Base.d_'45'__104
      d_'43''45'0'45'isAbelianGroup_3926
-- Data.Rational.Properties.+-mono-≤
d_'43''45'mono'45''8804'_3940 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'43''45'mono'45''8804'_3940 v0 v1 v2 v3 v4 v5
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204
      (coe
         MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v2))
      (coe
         MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
         (\ v6 v7 v8 ->
            coe
              MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'60''8658''8804'_366
              v8)
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v2)))
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v2)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45''60''45'trans_402)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v3)))
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v3)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3))))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'sym_134
                     (coe d_toℚ'7512''45'homo'45''43'_3706 (coe v1) (coe v3))))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'mono'45''8804'_1118
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v3))
                  (coe d_toℚ'7512''45'mono'45''8804'_3196 (coe v0) (coe v1) (coe v4))
                  (coe
                     d_toℚ'7512''45'mono'45''8804'_3196 (coe v2) (coe v3) (coe v5))))
            (d_toℚ'7512''45'homo'45''43'_3706 (coe v0) (coe v2))))
-- Data.Rational.Properties.+-monoˡ-≤
d_'43''45'mono'737''45''8804'_3962 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'43''45'mono'737''45''8804'_3962 v0 v1 v2 v3
  = coe
      d_'43''45'mono'45''8804'_3940 (coe v1) (coe v2) (coe v0) (coe v0)
      (coe v3) (coe d_'8804''45'refl_3280 (coe v0))
-- Data.Rational.Properties.+-monoʳ-≤
d_'43''45'mono'691''45''8804'_3970 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'43''45'mono'691''45''8804'_3970 v0 v1 v2 v3
  = coe
      d_'43''45'mono'45''8804'_3940 (coe v0) (coe v0) (coe v1) (coe v2)
      (coe d_'8804''45'refl_3280 (coe v0)) (coe v3)
-- Data.Rational.Properties.+-mono-<-≤
d_'43''45'mono'45''60''45''8804'_3976 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'43''45'mono'45''60''45''8804'_3976 v0 v1 v2 v3 v4 v5
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336
      (coe
         MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v2))
      (coe
         MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v2)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'trans_470)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'resp'45''8771'_572)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45''8804''45'trans_436)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v3)))
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v3)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3))))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'sym_134
                     (coe d_toℚ'7512''45'homo'45''43'_3706 (coe v1) (coe v3))))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'mono'45''60''45''8804'_1248
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v3))
                  (coe d_toℚ'7512''45'mono'45''60'_3328 (coe v0) (coe v1) (coe v4))
                  (coe
                     d_toℚ'7512''45'mono'45''8804'_3196 (coe v2) (coe v3) (coe v5))))
            (d_toℚ'7512''45'homo'45''43'_3706 (coe v0) (coe v2))))
-- Data.Rational.Properties.+-mono-≤-<
d_'43''45'mono'45''8804''45''60'_3994 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'43''45'mono'45''8804''45''60'_3994 v0 v1 v2 v3 v4 v5
  = coe
      d_'43''45'mono'45''60''45''8804'_3976 (coe v2) (coe v3) (coe v0)
      (coe v1) (coe v5) (coe v4)
-- Data.Rational.Properties.+-mono-<
d_'43''45'mono'45''60'_4016 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'43''45'mono'45''60'_4016 v0 v1 v2 v3 v4 v5
  = coe
      d_'60''45'trans_3454
      (MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v2))
      (MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v2))
      (MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v1) (coe v3))
      (d_'43''45'mono'45''60''45''8804'_3976
         (coe v0) (coe v1) (coe v2) (coe v2) (coe v4)
         (coe d_'8804''45'refl_3280 (coe v2)))
      (d_'43''45'mono'45''8804''45''60'_3994
         (coe v1) (coe v1) (coe v2) (coe v3)
         (coe d_'8804''45'refl_3280 (coe v1)) (coe v5))
-- Data.Rational.Properties.+-monoˡ-<
d_'43''45'mono'737''45''60'_4034 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'43''45'mono'737''45''60'_4034 v0 v1 v2 v3
  = coe
      d_'43''45'mono'45''60''45''8804'_3976 (coe v1) (coe v2) (coe v0)
      (coe v0) (coe v3) (coe d_'8804''45'refl_3280 (coe v0))
-- Data.Rational.Properties.+-monoʳ-<
d_'43''45'mono'691''45''60'_4042 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'43''45'mono'691''45''60'_4042 v0 v1 v2 v3
  = coe
      d_'43''45'mono'45''8804''45''60'_3994 (coe v0) (coe v0) (coe v1)
      (coe v2) (coe d_'8804''45'refl_3280 (coe v0)) (coe v3)
-- Data.Rational.Properties.*-nf
d_'42''45'nf_4048 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 -> Integer
d_'42''45'nf_4048 v0 v1
  = coe
      MAlonzo.Code.Data.Integer.GCD.d_gcd_132
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_numerator_14 (coe v1)))
      (coe
         MAlonzo.Code.Data.Integer.Base.d__'42'__308
         (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_denominator_22 (coe v1)))
-- Data.Rational.Properties.↥-*
d_'8613''45''42'_4058 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8613''45''42'_4058 = erased
-- Data.Rational.Properties.↧-*
d_'8615''45''42'_4068 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8615''45''42'_4068 = erased
-- Data.Rational.Properties.toℚᵘ-homo-*
d_toℚ'7512''45'homo'45''42'_4074 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_toℚ'7512''45'homo'45''42'_4074 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6
               -> let v8
                        = MAlonzo.Code.Data.Integer.Properties.d__'8799'__2476
                            (coe
                               d_'42''45'nf_4048
                               (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3)
                               (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6))
                            (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12) in
                  case coe v8 of
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                      -> coe
                           seq (coe v9)
                           (coe
                              seq (coe v10)
                              (coe
                                 MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties._.eq2
d_eq2_4094 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_eq2_4094 = erased
-- Data.Rational.Properties._.eq
d_eq_4096 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_eq_4096 = erased
-- Data.Rational.Properties.toℚᵘ-homo-1/
d_toℚ'7512''45'homo'45'1'47'_4196 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_toℚ'7512''45'homo'45'1'47'_4196 v0 ~v1
  = du_toℚ'7512''45'homo'45'1'47'_4196 v0
du_toℚ'7512''45'homo'45'1'47'_4196 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
du_toℚ'7512''45'homo'45'1'47'_4196 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'refl_130)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.toℚᵘ-isMagmaHomomorphism-*
d_toℚ'7512''45'isMagmaHomomorphism'45''42'_4198 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_toℚ'7512''45'isMagmaHomomorphism'45''42'_4198
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaHomomorphism'46'constructor_1049
      (coe d_toℚ'7512''45'isRelHomomorphism_3188)
      (coe d_toℚ'7512''45'homo'45''42'_4074)
-- Data.Rational.Properties.toℚᵘ-isMonoidHomomorphism-*
d_toℚ'7512''45'isMonoidHomomorphism'45''42'_4200 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidHomomorphism_266
d_toℚ'7512''45'isMonoidHomomorphism'45''42'_4200
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMonoidHomomorphism'46'constructor_5533
      (coe d_toℚ'7512''45'isMagmaHomomorphism'45''42'_4198)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'refl_130)
-- Data.Rational.Properties.toℚᵘ-isMonoidMonomorphism-*
d_toℚ'7512''45'isMonoidMonomorphism'45''42'_4202 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288
d_toℚ'7512''45'isMonoidMonomorphism'45''42'_4202
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMonoidMonomorphism'46'constructor_6057
      (coe d_toℚ'7512''45'isMonoidHomomorphism'45''42'_4200) erased
-- Data.Rational.Properties.toℚᵘ-isNearSemiringHomomorphism-+-*
d_toℚ'7512''45'isNearSemiringHomomorphism'45''43''45''42'_4204 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsNearSemiringHomomorphism_864
d_toℚ'7512''45'isNearSemiringHomomorphism'45''43''45''42'_4204
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsNearSemiringHomomorphism'46'constructor_15507
      (coe d_toℚ'7512''45'isMonoidHomomorphism'45''43'_3826)
      (coe d_toℚ'7512''45'homo'45''42'_4074)
-- Data.Rational.Properties.toℚᵘ-isNearSemiringMonomorphism-+-*
d_toℚ'7512''45'isNearSemiringMonomorphism'45''43''45''42'_4206 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsNearSemiringMonomorphism_892
d_toℚ'7512''45'isNearSemiringMonomorphism'45''43''45''42'_4206
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsNearSemiringMonomorphism'46'constructor_16335
      (coe
         d_toℚ'7512''45'isNearSemiringHomomorphism'45''43''45''42'_4204)
      erased
-- Data.Rational.Properties.toℚᵘ-isSemiringHomomorphism-+-*
d_toℚ'7512''45'isSemiringHomomorphism'45''43''45''42'_4208 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsSemiringHomomorphism_1246
d_toℚ'7512''45'isSemiringHomomorphism'45''43''45''42'_4208
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsSemiringHomomorphism'46'constructor_21773
      (coe
         d_toℚ'7512''45'isNearSemiringHomomorphism'45''43''45''42'_4204)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'refl_130)
-- Data.Rational.Properties.toℚᵘ-isSemiringMonomorphism-+-*
d_toℚ'7512''45'isSemiringMonomorphism'45''43''45''42'_4210 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsSemiringMonomorphism_1280
d_toℚ'7512''45'isSemiringMonomorphism'45''43''45''42'_4210
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsSemiringMonomorphism'46'constructor_22781
      (coe d_toℚ'7512''45'isSemiringHomomorphism'45''43''45''42'_4208)
      erased
-- Data.Rational.Properties.toℚᵘ-isRingHomomorphism-+-*
d_toℚ'7512''45'isRingHomomorphism'45''43''45''42'_4212 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingHomomorphism_2124
d_toℚ'7512''45'isRingHomomorphism'45''43''45''42'_4212
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsRingHomomorphism'46'constructor_36047
      (coe d_toℚ'7512''45'isSemiringHomomorphism'45''43''45''42'_4208)
      (coe d_toℚ'7512''45'homo'8255''45'_3830)
-- Data.Rational.Properties.toℚᵘ-isRingMonomorphism-+-*
d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164
d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsRingMonomorphism'46'constructor_37231
      (coe d_toℚ'7512''45'isRingHomomorphism'45''43''45''42'_4212) erased
-- Data.Rational.Properties.+-*-Monomorphism.assoc
d_assoc_4218 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_4218 = erased
-- Data.Rational.Properties.+-*-Monomorphism.comm
d_comm_4226 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_4226 = erased
-- Data.Rational.Properties.+-*-Monomorphism.identityʳ
d_identity'691'_4234 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identity'691'_4234 = erased
-- Data.Rational.Properties.+-*-Monomorphism.identityˡ
d_identity'737'_4236 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identity'737'_4236 = erased
-- Data.Rational.Properties.+-*-Monomorphism.isCommutativeMonoid
d_isCommutativeMonoid_4240 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_4240
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Data.Rational.Properties.+-*-Monomorphism.isMagma
d_isMagma_4242 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_4242
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Data.Rational.Properties.+-*-Monomorphism.isMonoid
d_isMonoid_4244 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_4244
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Data.Rational.Properties.+-*-Monomorphism.isSemigroup
d_isSemigroup_4248 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_4248
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Data.Rational.Properties.+-*-Monomorphism.assoc
d_assoc_4258 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_assoc_4258 = erased
-- Data.Rational.Properties.+-*-Monomorphism.comm
d_comm_4266 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_comm_4266 = erased
-- Data.Rational.Properties.+-*-Monomorphism.identityʳ
d_identity'691'_4274 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identity'691'_4274 = erased
-- Data.Rational.Properties.+-*-Monomorphism.identityˡ
d_identity'737'_4276 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identity'737'_4276 = erased
-- Data.Rational.Properties.+-*-Monomorphism.isCommutativeMonoid
d_isCommutativeMonoid_4280 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_4280
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Data.Rational.Properties.+-*-Monomorphism.isMagma
d_isMagma_4282 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_4282
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Data.Rational.Properties.+-*-Monomorphism.isMonoid
d_isMonoid_4284 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_4284
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Data.Rational.Properties.+-*-Monomorphism.isSemigroup
d_isSemigroup_4288 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_4288
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Data.Rational.Properties.+-*-Monomorphism.distribʳ
d_distrib'691'_4300 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'691'_4300 = erased
-- Data.Rational.Properties.+-*-Monomorphism.distribˡ
d_distrib'737'_4302 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'737'_4302 = erased
-- Data.Rational.Properties.+-*-Monomorphism.inverse
d_inverse_4304 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_4304
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_inverse_190
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Data.Rational.Properties.+-*-Monomorphism.inverseʳ
d_inverse'691'_4306 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inverse'691'_4306 = erased
-- Data.Rational.Properties.+-*-Monomorphism.inverseˡ
d_inverse'737'_4308 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_inverse'737'_4308 = erased
-- Data.Rational.Properties.+-*-Monomorphism.isAbelianGroup
d_isAbelianGroup_4310 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_isAbelianGroup_4310
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isAbelianGroup_382
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Data.Rational.Properties.+-*-Monomorphism.isCommutativeRing
d_isCommutativeRing_4312 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_isCommutativeRing_4312
  = coe
      MAlonzo.Code.Algebra.Morphism.RingMonomorphism.du_isCommutativeRing_514
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214)
-- Data.Rational.Properties.+-*-Monomorphism.isGroup
d_isGroup_4314 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_4314
  = let v0
          = MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382 in
    let v1
          = MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312 in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3 = d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214 in
    coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isGroup_318
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Data.Rational.Properties.+-*-Monomorphism.isRing
d_isRing_4316 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_4316
  = coe
      MAlonzo.Code.Algebra.Morphism.RingMonomorphism.du_isRing_398
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214)
-- Data.Rational.Properties.+-*-Monomorphism.⁻¹-cong
d_'8315''185''45'cong_4318 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'cong_4318 = erased
-- Data.Rational.Properties.+-*-Monomorphism.neg-distribʳ-*
d_neg'45'distrib'691''45''42'_4320 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'691''45''42'_4320 = erased
-- Data.Rational.Properties.+-*-Monomorphism.neg-distribˡ-*
d_neg'45'distrib'737''45''42'_4322 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'737''45''42'_4322 = erased
-- Data.Rational.Properties.+-*-Monomorphism.zeroʳ
d_zero'691'_4326 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zero'691'_4326 = erased
-- Data.Rational.Properties.+-*-Monomorphism.zeroˡ
d_zero'737'_4328 ::
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_zero'737'_4328 = erased
-- Data.Rational.Properties.+-*-Monomorphism.⁻¹-distrib-∙
d_'8315''185''45'distrib'45''8729'_4330 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  (MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T_ℚ'7512'_8 ->
   MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8315''185''45'distrib'45''8729'_4330 = erased
-- Data.Rational.Properties.*-assoc
d_'42''45'assoc_4332 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'assoc_4332 = erased
-- Data.Rational.Properties.*-comm
d_'42''45'comm_4334 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'comm_4334 = erased
-- Data.Rational.Properties.*-identityˡ
d_'42''45'identity'737'_4336 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'737'_4336 = erased
-- Data.Rational.Properties.*-identityʳ
d_'42''45'identity'691'_4338 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'identity'691'_4338 = erased
-- Data.Rational.Properties.*-identity
d_'42''45'identity_4340 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_4340
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Properties.*-zeroˡ
d_'42''45'zero'737'_4342 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'zero'737'_4342 = erased
-- Data.Rational.Properties.*-zeroʳ
d_'42''45'zero'691'_4344 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'zero'691'_4344 = erased
-- Data.Rational.Properties.*-zero
d_'42''45'zero_4346 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'zero_4346
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Properties.*-distribˡ-+
d_'42''45'distrib'737''45''43'_4348 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''43'_4348 = erased
-- Data.Rational.Properties.*-distribʳ-+
d_'42''45'distrib'691''45''43'_4350 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''43'_4350 = erased
-- Data.Rational.Properties.*-distrib-+
d_'42''45'distrib'45''43'_4352 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'distrib'45''43'_4352
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Rational.Properties.*-inverseˡ
d_'42''45'inverse'737'_4358 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'inverse'737'_4358 = erased
-- Data.Rational.Properties.*-inverseʳ
d_'42''45'inverse'691'_4370 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'inverse'691'_4370 = erased
-- Data.Rational.Properties.neg-distribˡ-*
d_neg'45'distrib'737''45''42'_4378 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'737''45''42'_4378 = erased
-- Data.Rational.Properties.neg-distribʳ-*
d_neg'45'distrib'691''45''42'_4384 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_neg'45'distrib'691''45''42'_4384 = erased
-- Data.Rational.Properties.*-isMagma
d_'42''45'isMagma_4386 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_4386
  = let v0
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                 (coe
                    MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)) in
    let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                 (coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)) in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'42''45'isMagma_2216)
-- Data.Rational.Properties.*-isSemigroup
d_'42''45'isSemigroup_4388 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_4388
  = let v0
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                 (coe
                    MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)) in
    let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                 (coe
                    MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)) in
    let v2 = MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 in
    let v3
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'42''45'isSemigroup_2218)
-- Data.Rational.Properties.*-1-isMonoid
d_'42''45'1'45'isMonoid_4390 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'1'45'isMonoid_4390
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
            (coe
               MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)))
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'42''45'1'45'isMonoid_2220)
-- Data.Rational.Properties.*-1-isCommutativeMonoid
d_'42''45'1'45'isCommutativeMonoid_4392 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'1'45'isCommutativeMonoid_4392
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
            (coe
               MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)))
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214))
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'42''45'1'45'isCommutativeMonoid_2222)
-- Data.Rational.Properties.+-*-isRing
d_'43''45''42''45'isRing_4394 ::
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_'43''45''42''45'isRing_4394
  = coe
      MAlonzo.Code.Algebra.Morphism.RingMonomorphism.du_isRing_398
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45''42''45'isRing_2224)
-- Data.Rational.Properties.+-*-isCommutativeRing
d_'43''45''42''45'isCommutativeRing_4396 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_'43''45''42''45'isCommutativeRing_4396
  = coe
      MAlonzo.Code.Algebra.Morphism.RingMonomorphism.du_isCommutativeRing_514
      (coe MAlonzo.Code.Data.Rational.Base.d_'43''45''42''45'rawRing_382)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'43''45''42''45'rawRing_312)
      (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158)
      (coe d_toℚ'7512''45'isRingMonomorphism'45''43''45''42'_4214)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45''42''45'isCommutativeRing_2226)
-- Data.Rational.Properties.*-magma
d_'42''45'magma_4398 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'42''45'magma_4398
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      MAlonzo.Code.Data.Rational.Base.d__'42'__266 d_'42''45'isMagma_4386
-- Data.Rational.Properties.*-semigroup
d_'42''45'semigroup_4400 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'42''45'semigroup_4400
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      MAlonzo.Code.Data.Rational.Base.d__'42'__266
      d_'42''45'isSemigroup_4388
-- Data.Rational.Properties.*-1-monoid
d_'42''45'1'45'monoid_4402 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'1'45'monoid_4402
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      MAlonzo.Code.Data.Rational.Base.d__'42'__266
      MAlonzo.Code.Data.Rational.Base.d_1ℚ_172
      d_'42''45'1'45'isMonoid_4390
-- Data.Rational.Properties.*-1-commutativeMonoid
d_'42''45'1'45'commutativeMonoid_4404 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'1'45'commutativeMonoid_4404
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      MAlonzo.Code.Data.Rational.Base.d__'42'__266
      MAlonzo.Code.Data.Rational.Base.d_1ℚ_172
      d_'42''45'1'45'isCommutativeMonoid_4392
-- Data.Rational.Properties.+-*-ring
d_'43''45''42''45'ring_4406 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432
d_'43''45''42''45'ring_4406
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Ring'46'constructor_60565
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      MAlonzo.Code.Data.Rational.Base.d__'42'__266
      MAlonzo.Code.Data.Rational.Base.d_'45'__104
      MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
      MAlonzo.Code.Data.Rational.Base.d_1ℚ_172
      d_'43''45''42''45'isRing_4394
-- Data.Rational.Properties.+-*-commutativeRing
d_'43''45''42''45'commutativeRing_4408 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
d_'43''45''42''45'commutativeRing_4408
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeRing'46'constructor_64147
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      MAlonzo.Code.Data.Rational.Base.d__'42'__266
      MAlonzo.Code.Data.Rational.Base.d_'45'__104
      MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
      MAlonzo.Code.Data.Rational.Base.d_1ℚ_172
      d_'43''45''42''45'isCommutativeRing_4396
-- Data.Rational.Properties.*-cancelʳ-≤-pos
d_'42''45'cancel'691''45''8804''45'pos_4414 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'cancel'691''45''8804''45'pos_4414 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'691''45''8804''45'pos_4414 v0 v1 v2 v4
du_'42''45'cancel'691''45''8804''45'pos_4414 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'cancel'691''45''8804''45'pos_4414 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'cancel'691''45''8804''45'pos_1796
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
            (\ v4 v5 v6 ->
               coe
                 MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'60''8658''8804'_366
                 v6)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
               (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2)))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45''60''45'trans_402)
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v2)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))))
                     (d_toℚ'7512''45'homo'45''42'_4074 (coe v1) (coe v2)))
                  (coe
                     d_toℚ'7512''45'mono'45''8804'_3196
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2))
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v2))
                     (coe v3)))
               (d_toℚ'7512''45'homo'45''42'_4074 (coe v0) (coe v2)))))
-- Data.Rational.Properties.*-cancelˡ-≤-pos
d_'42''45'cancel'737''45''8804''45'pos_4432 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'cancel'737''45''8804''45'pos_4432 v0 v1 v2 ~v3
  = du_'42''45'cancel'737''45''8804''45'pos_4432 v0 v1 v2
du_'42''45'cancel'737''45''8804''45'pos_4432 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'cancel'737''45''8804''45'pos_4432 v0 v1 v2
  = coe
      du_'42''45'cancel'691''45''8804''45'pos_4414 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Properties.*-monoʳ-≤-nonNeg
d_'42''45'mono'691''45''8804''45'nonNeg_4454 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'691''45''8804''45'nonNeg_4454 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'691''45''8804''45'nonNeg_4454 v0 v2 v3 v4
du_'42''45'mono'691''45''8804''45'nonNeg_4454 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'691''45''8804''45'nonNeg_4454 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         (\ v4 v5 -> v4) v1 v2)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5)
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         v1 v2)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
         (\ v4 v5 v6 ->
            coe
              MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'60''8658''8804'_366
              v6)
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
               (\ v4 ->
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
               (\ v4 v5 -> v4) v1 v2))
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
               (\ v4 v5 -> v5)
               (\ v4 ->
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
               v1 v2))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                  (\ v4 v5 -> v5)
                  (\ v4 ->
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
                  v1 v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45''60''45'trans_402)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v4 v5 -> v5)
                     (\ v4 ->
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
                     v1 v2))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                        (\ v4 v5 -> v5)
                        (\ v4 ->
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
                        v1 v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0))))
                  (d_toℚ'7512''45'homo'45''42'_4074 (coe v2) (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'mono'737''45''8804''45'nonNeg_1874
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe
                     d_toℚ'7512''45'mono'45''8804'_3196 (coe v1) (coe v2) (coe v3))))
            (d_toℚ'7512''45'homo'45''42'_4074 (coe v1) (coe v0))))
-- Data.Rational.Properties.*-monoˡ-≤-nonNeg
d_'42''45'mono'737''45''8804''45'nonNeg_4474 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'737''45''8804''45'nonNeg_4474 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804''45'nonNeg_4474 v0 v2 v3
du_'42''45'mono'737''45''8804''45'nonNeg_4474 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'737''45''8804''45'nonNeg_4474 v0 v1 v2
  = coe
      du_'42''45'mono'691''45''8804''45'nonNeg_4454 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Properties.*-monoʳ-≤-nonPos
d_'42''45'mono'691''45''8804''45'nonPos_4496 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'691''45''8804''45'nonPos_4496 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'691''45''8804''45'nonPos_4496 v0 v2 v3 v4
du_'42''45'mono'691''45''8804''45'nonPos_4496 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'691''45''8804''45'nonPos_4496 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5)
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         v1 v2)
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         (\ v4 v5 -> v4) v1 v2)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
         (\ v4 v5 v6 ->
            coe
              MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'60''8658''8804'_366
              v6)
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
               (\ v4 v5 -> v5)
               (\ v4 ->
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
               v1 v2))
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
               (\ v4 ->
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
               (\ v4 v5 -> v4) v1 v2))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                  (\ v4 ->
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
                  (\ v4 v5 -> v4) v1 v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45''60''45'trans_402)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v4 ->
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
                     (\ v4 v5 -> v4) v1 v2))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                        (\ v4 ->
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
                        (\ v4 v5 -> v4) v1 v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0))))
                  (d_toℚ'7512''45'homo'45''42'_4074 (coe v1) (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'mono'737''45''8804''45'nonPos_1956
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe
                     d_toℚ'7512''45'mono'45''8804'_3196 (coe v1) (coe v2) (coe v3))))
            (d_toℚ'7512''45'homo'45''42'_4074 (coe v2) (coe v0))))
-- Data.Rational.Properties.*-monoˡ-≤-nonPos
d_'42''45'mono'737''45''8804''45'nonPos_4516 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'737''45''8804''45'nonPos_4516 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804''45'nonPos_4516 v0 v2 v3
du_'42''45'mono'737''45''8804''45'nonPos_4516 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'737''45''8804''45'nonPos_4516 v0 v1 v2
  = coe
      du_'42''45'mono'691''45''8804''45'nonPos_4496 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Properties.*-cancelʳ-≤-neg
d_'42''45'cancel'691''45''8804''45'neg_4536 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'cancel'691''45''8804''45'neg_4536 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'691''45''8804''45'neg_4536 v0 v1 v2 v4
du_'42''45'cancel'691''45''8804''45'neg_4536 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'cancel'691''45''8804''45'neg_4536 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204 (coe v1) (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'cancel'691''45''8804''45'neg_1834
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
            (\ v4 v5 v6 ->
               coe
                 MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'60''8658''8804'_366
                 v6)
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
               (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2)))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45''60''45'trans_402)
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v2)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))))
                     (d_toℚ'7512''45'homo'45''42'_4074 (coe v1) (coe v2)))
                  (coe
                     d_toℚ'7512''45'mono'45''8804'_3196
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2))
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v2))
                     (coe v3)))
               (d_toℚ'7512''45'homo'45''42'_4074 (coe v0) (coe v2)))))
-- Data.Rational.Properties.*-cancelˡ-≤-neg
d_'42''45'cancel'737''45''8804''45'neg_4554 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'cancel'737''45''8804''45'neg_4554 v0 v1 v2 ~v3
  = du_'42''45'cancel'737''45''8804''45'neg_4554 v0 v1 v2
du_'42''45'cancel'737''45''8804''45'neg_4554 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'cancel'737''45''8804''45'neg_4554 v0 v1 v2
  = coe
      du_'42''45'cancel'691''45''8804''45'neg_4536 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Properties.*-monoˡ-<-pos
d_'42''45'mono'737''45''60''45'pos_4576 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'mono'737''45''60''45'pos_4576 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''60''45'pos_4576 v0 v2 v3 v4
du_'42''45'mono'737''45''60''45'pos_4576 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'mono'737''45''60''45'pos_4576 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         (\ v4 v5 -> v4) v1 v2)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5)
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         v1 v2)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'trans_470)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'resp'45''8771'_572)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45''8804''45'trans_436)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0))))
                  (d_toℚ'7512''45'homo'45''42'_4074 (coe v2) (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'mono'737''45''60''45'pos_2000
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe d_toℚ'7512''45'mono'45''60'_3328 (coe v1) (coe v2) (coe v3))))
            (d_toℚ'7512''45'homo'45''42'_4074 (coe v1) (coe v0))))
-- Data.Rational.Properties.*-monoʳ-<-pos
d_'42''45'mono'691''45''60''45'pos_4596 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'mono'691''45''60''45'pos_4596 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''60''45'pos_4596 v0 v2 v3
du_'42''45'mono'691''45''60''45'pos_4596 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'mono'691''45''60''45'pos_4596 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''60''45'pos_4576 (coe v0) (coe v1) (coe v2)
-- Data.Rational.Properties.*-cancelˡ-<-nonNeg
d_'42''45'cancel'737''45''60''45'nonNeg_4620 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'737''45''60''45'nonNeg_4620 v0 ~v1 v2 v3 v4
  = du_'42''45'cancel'737''45''60''45'nonNeg_4620 v0 v2 v3 v4
du_'42''45'cancel'737''45''60''45'nonNeg_4620 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'737''45''60''45'nonNeg_4620 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336 (coe v1) (coe v2)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'cancel'737''45''60''45'nonNeg_2088
         (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
         (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
         (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
               (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v1)))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'trans_470)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'resp'45''8771'_572)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45''8804''45'trans_436)
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v1)))
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))))
                     (d_toℚ'7512''45'homo'45''42'_4074 (coe v0) (coe v2)))
                  (coe
                     d_toℚ'7512''45'mono'45''60'_3328
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v1))
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v0) (coe v2))
                     (coe v3)))
               (d_toℚ'7512''45'homo'45''42'_4074 (coe v0) (coe v1)))))
-- Data.Rational.Properties.*-cancelʳ-<-nonNeg
d_'42''45'cancel'691''45''60''45'nonNeg_4642 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'691''45''60''45'nonNeg_4642 v0 ~v1 v2 v3
  = du_'42''45'cancel'691''45''60''45'nonNeg_4642 v0 v2 v3
du_'42''45'cancel'691''45''60''45'nonNeg_4642 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'691''45''60''45'nonNeg_4642 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''60''45'nonNeg_4620 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Properties.*-monoˡ-<-neg
d_'42''45'mono'737''45''60''45'neg_4664 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'mono'737''45''60''45'neg_4664 v0 ~v1 v2 v3 v4
  = du_'42''45'mono'737''45''60''45'neg_4664 v0 v2 v3 v4
du_'42''45'mono'737''45''60''45'neg_4664 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'mono'737''45''60''45'neg_4664 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v4 v5 -> v5)
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         v1 v2)
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (\ v4 ->
            MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v4) (coe v0))
         (\ v4 v5 -> v4) v1 v2)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
               (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'trans_470)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'resp'45''8771'_572)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45''8804''45'trans_436)
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
                  (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0)))
                  (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v1) (coe v0))))
                  (d_toℚ'7512''45'homo'45''42'_4074 (coe v1) (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'mono'737''45''60''45'neg_2110
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe d_toℚ'7512''45'mono'45''60'_3328 (coe v1) (coe v2) (coe v3))))
            (d_toℚ'7512''45'homo'45''42'_4074 (coe v2) (coe v0))))
-- Data.Rational.Properties.*-monoʳ-<-neg
d_'42''45'mono'691''45''60''45'neg_4684 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'mono'691''45''60''45'neg_4684 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''60''45'neg_4684 v0 v2 v3
du_'42''45'mono'691''45''60''45'neg_4684 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'mono'691''45''60''45'neg_4684 v0 v1 v2
  = coe
      du_'42''45'mono'737''45''60''45'neg_4664 (coe v0) (coe v1) (coe v2)
-- Data.Rational.Properties.*-cancelˡ-<-nonPos
d_'42''45'cancel'737''45''60''45'nonPos_4704 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'737''45''60''45'nonPos_4704 v0 v1 v2 ~v3 v4
  = du_'42''45'cancel'737''45''60''45'nonPos_4704 v0 v1 v2 v4
du_'42''45'cancel'737''45''60''45'nonPos_4704 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'737''45''60''45'nonPos_4704 v0 v1 v2 v3
  = coe
      d_toℚ'7512''45'cancel'45''60'_3336 (coe v1) (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'42''45'cancel'737''45''60''45'nonPos_2152
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'trans_470)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45'resp'45''8771'_572)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'60''45''8804''45'trans_436)
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0)))
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v1)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
                     (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v1)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'42'__202
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v2))
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))))
                     (d_toℚ'7512''45'homo'45''42'_4074 (coe v2) (coe v1)))
                  (coe
                     d_toℚ'7512''45'mono'45''60'_3328
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v0))
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'42'__266 (coe v2) (coe v1))
                     (coe v3)))
               (d_toℚ'7512''45'homo'45''42'_4074 (coe v2) (coe v0)))))
-- Data.Rational.Properties.*-cancelʳ-<-nonPos
d_'42''45'cancel'691''45''60''45'nonPos_4722 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'691''45''60''45'nonPos_4722 v0 v1 v2 ~v3
  = du_'42''45'cancel'691''45''60''45'nonPos_4722 v0 v1 v2
du_'42''45'cancel'691''45''60''45'nonPos_4722 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'691''45''60''45'nonPos_4722 v0 v1 v2
  = coe
      du_'42''45'cancel'737''45''60''45'nonPos_4704 (coe v0) (coe v1)
      (coe v2)
-- Data.Rational.Properties.p≤q⇒p⊔q≡q
d_p'8804'q'8658'p'8852'q'8801'q_4738 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p'8804'q'8658'p'8852'q'8801'q_4738 = erased
-- Data.Rational.Properties.p≥q⇒p⊔q≡p
d_p'8805'q'8658'p'8852'q'8801'p_4766 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p'8805'q'8658'p'8852'q'8801'p_4766 = erased
-- Data.Rational.Properties.p≤q⇒p⊓q≡p
d_p'8804'q'8658'p'8851'q'8801'p_4796 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p'8804'q'8658'p'8851'q'8801'p_4796 = erased
-- Data.Rational.Properties.p≥q⇒p⊓q≡q
d_p'8805'q'8658'p'8851'q'8801'q_4824 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p'8805'q'8658'p'8851'q'8801'q_4824 = erased
-- Data.Rational.Properties.⊓-operator
d_'8851''45'operator_4854 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84
d_'8851''45'operator_4854
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MinOperator'46'constructor_973
      (coe MAlonzo.Code.Data.Rational.Base.d__'8851'__322) erased erased
-- Data.Rational.Properties.⊔-operator
d_'8852''45'operator_4856 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MaxOperator_114
d_'8852''45'operator_4856
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MaxOperator'46'constructor_1501
      (coe MAlonzo.Code.Data.Rational.Base.d__'8852'__312) erased erased
-- Data.Rational.Properties.⊓-⊔-properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_4860 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8851'_4860 = erased
-- Data.Rational.Properties.⊓-⊔-properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_4862 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8852'_4862 = erased
-- Data.Rational.Properties.⊓-⊔-properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_4864 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8851'_4864 = erased
-- Data.Rational.Properties.⊓-⊔-properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_4866 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8852'_4866 = erased
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_4868 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8804'x_4868
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_4870 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8658'x'8851'z'8804'y_4870
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_4872 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8658'z'8851'x'8804'y_4872
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_4874 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8658'x'8851'z'8804'y_4874
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_4876 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8658'z'8851'x'8804'y_4876
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_4878 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8851'z'8658'x'8804'y_4878
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_4880 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8851'z'8658'x'8804'z_4880
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_4882 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8804'y_4882
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_4884 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8776'x'8658'x'8804'y_4884
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_4886 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8776'y'8658'y'8804'x_4886
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≤x
d_x'8851'y'8804'x_4888 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8804'x_4888
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≤x⊔y
d_x'8851'y'8804'x'8852'y_4890 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8804'x'8852'y_4890
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_x'8851'y'8804'x'8852'y_2996
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≤y
d_x'8851'y'8804'y_4892 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8804'y_4892
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_4894 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8776'x'8658'x'8804'y_4894
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_4896 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8851'y'8776'y'8658'y'8804'x_4896
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_4898 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8851'z'8658'x'8804'y_4898
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_4900 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_x'8804'y'8851'z'8658'x'8804'z_4900
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-absorbs-⊔
d_'8851''45'absorbs'45''8852'_4902 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'absorbs'45''8852'_4902 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_4904 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'assoc_4904 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_4906 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_4906
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_4908 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'comm_4908 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_4910 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_4910
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-distrib-⊔
d_'8851''45'distrib'45''8852'_4918 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'distrib'45''8852'_4918
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45'distrib'45''8852'_2816
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-distribʳ-⊔
d_'8851''45'distrib'691''45''8852'_4920 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'distrib'691''45''8852'_4920 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-distribˡ-⊔
d_'8851''45'distrib'737''45''8852'_4922 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'distrib'737''45''8852'_4922 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_4924 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'glb_4924
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_4926 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'idem_4926 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_4934 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_4934
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_4936 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_4936
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_4938 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_4938
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_4942 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_4942
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_4944 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_4944
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_4946 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_4946
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_4948 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'mono'45''8804'_4948
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_4952 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'mono'691''45''8804'_4952
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_4954 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'mono'737''45''8804'_4954
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_4958 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_4958
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_4960 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_4960
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_4962 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_4962
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_4964 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'triangulate_4964 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-⊔-absorptive
d_'8851''45''8852''45'absorptive_4972 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45''8852''45'absorptive_4972
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'absorptive_2896
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-properties.⊔-absorbs-⊓
d_'8852''45'absorbs'45''8851'_4974 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'absorbs'45''8851'_4974 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-assoc
d_'8851''45'assoc_4976 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'assoc_4976 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-band
d_'8851''45'band_4978 :: MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_4978
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-comm
d_'8851''45'comm_4980 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'comm_4980 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_4982 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_4982
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊔-distrib-⊓
d_'8852''45'distrib'45''8851'_4990 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45'distrib'45''8851'_4990
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45'distrib'45''8851'_2848
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-properties.⊔-distribʳ-⊓
d_'8852''45'distrib'691''45''8851'_4992 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'distrib'691''45''8851'_4992 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊔-distribˡ-⊓
d_'8852''45'distrib'737''45''8851'_4994 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8852''45'distrib'737''45''8851'_4994 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-idem
d_'8851''45'idem_4996 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'idem_4996 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isBand
d_'8851''45'isBand_5004 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_5004
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_5006 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_5006
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isMagma
d_'8851''45'isMagma_5008 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_5008
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_5012 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_5012
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-isSemigroup
d_'8851''45'isSemigroup_5014 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_5014
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-glb
d_'8851''45'glb_5016 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'glb_5016
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-magma
d_'8851''45'magma_5018 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_5018
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-mono-≤
d_'8851''45'mono'45''8804'_5020 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'mono'45''8804'_5020
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_5024 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'mono'691''45''8804'_5024
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_5026 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8851''45'mono'737''45''8804'_5026
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-sel
d_'8851''45'sel_5028 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_5028
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-selectiveMagma
d_'8851''45'selectiveMagma_5030 ::
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_5030
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-semigroup
d_'8851''45'semigroup_5032 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_5032
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-properties.⊓-triangulate
d_'8851''45'triangulate_5034 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8851''45'triangulate_5034 = erased
-- Data.Rational.Properties.⊓-⊔-properties.⊔-⊓-absorptive
d_'8852''45''8851''45'absorptive_5042 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8852''45''8851''45'absorptive_5042
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'absorptive_2894
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_5046 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_5046
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_5048 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_5048
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8851''45'operator_4854 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe v0) (coe v1)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-⊔-distributiveLattice
d_'8851''45''8852''45'distributiveLattice_5050 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8851''45''8852''45'distributiveLattice_5050
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'distributiveLattice_770
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-⊔-isDistributiveLattice
d_'8851''45''8852''45'isDistributiveLattice_5052 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8851''45''8852''45'isDistributiveLattice_5052
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isDistributiveLattice_760
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-⊔-isLattice
d_'8851''45''8852''45'isLattice_5054 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8851''45''8852''45'isLattice_5054
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'isLattice_758
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-⊔-lattice
d_'8851''45''8852''45'lattice_5056 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8851''45''8852''45'lattice_5056
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8851''45''8852''45'lattice_766
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-isSemilattice
d_'8851''45'isSemilattice_5058 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8851''45'isSemilattice_5058
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'isSemilattice_586
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊓-semilattice
d_'8851''45'semilattice_5060 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8851''45'semilattice_5060
  = let v0 = d_'8804''45'totalPreorder_3320 in
    let v1 = d_'8852''45'operator_4856 in
    coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinOp.du_'8851''45'semilattice_588
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v1))
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊔-⊓-distributiveLattice
d_'8852''45''8851''45'distributiveLattice_5062 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8852''45''8851''45'distributiveLattice_5062
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'distributiveLattice_768
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊔-⊓-isDistributiveLattice
d_'8852''45''8851''45'isDistributiveLattice_5064 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8852''45''8851''45'isDistributiveLattice_5064
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isDistributiveLattice_762
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊔-⊓-isLattice
d_'8852''45''8851''45'isLattice_5066 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8852''45''8851''45'isLattice_5066
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'isLattice_756
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.⊓-⊔-latticeProperties.⊔-⊓-lattice
d_'8852''45''8851''45'lattice_5068 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8852''45''8851''45'lattice_5068
  = coe
      MAlonzo.Code.Algebra.Lattice.Construct.NaturalChoice.MinMaxOp.du_'8852''45''8851''45'lattice_764
      (coe d_'8804''45'totalPreorder_3320)
      (coe d_'8851''45'operator_4854) (coe d_'8852''45'operator_4856)
-- Data.Rational.Properties.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_5076 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8852'_5076 = erased
-- Data.Rational.Properties.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_5086 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''8804''45'distrib'45''8851'_5086 = erased
-- Data.Rational.Properties.mono-<-distrib-⊓
d_mono'45''60''45'distrib'45''8851'_5096 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
   MAlonzo.Code.Data.Rational.Base.T__'60'__54) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''60''45'distrib'45''8851'_5096 = erased
-- Data.Rational.Properties.mono-<-distrib-⊔
d_mono'45''60''45'distrib'45''8852'_5168 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
   MAlonzo.Code.Data.Rational.Base.T__'60'__54) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_mono'45''60''45'distrib'45''8852'_5168 = erased
-- Data.Rational.Properties.antimono-≤-distrib-⊓
d_antimono'45''8804''45'distrib'45''8851'_5240 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8851'_5240 = erased
-- Data.Rational.Properties.antimono-≤-distrib-⊔
d_antimono'45''8804''45'distrib'45''8852'_5250 ::
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6) ->
  (MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
   MAlonzo.Code.Data.Rational.Base.T__'8804'__46) ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antimono'45''8804''45'distrib'45''8852'_5250 = erased
-- Data.Rational.Properties.*-distribˡ-⊓-nonNeg
d_'42''45'distrib'737''45''8851''45'nonNeg_5262 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8851''45'nonNeg_5262 = erased
-- Data.Rational.Properties.*-distribʳ-⊓-nonNeg
d_'42''45'distrib'691''45''8851''45'nonNeg_5274 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8851''45'nonNeg_5274 = erased
-- Data.Rational.Properties.*-distribˡ-⊔-nonNeg
d_'42''45'distrib'737''45''8852''45'nonNeg_5286 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8852''45'nonNeg_5286 = erased
-- Data.Rational.Properties.*-distribʳ-⊔-nonNeg
d_'42''45'distrib'691''45''8852''45'nonNeg_5298 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8852''45'nonNeg_5298 = erased
-- Data.Rational.Properties.*-distribˡ-⊔-nonPos
d_'42''45'distrib'737''45''8852''45'nonPos_5310 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8852''45'nonPos_5310 = erased
-- Data.Rational.Properties.*-distribʳ-⊔-nonPos
d_'42''45'distrib'691''45''8852''45'nonPos_5322 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8852''45'nonPos_5322 = erased
-- Data.Rational.Properties.*-distribˡ-⊓-nonPos
d_'42''45'distrib'737''45''8851''45'nonPos_5334 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'737''45''8851''45'nonPos_5334 = erased
-- Data.Rational.Properties.*-distribʳ-⊓-nonPos
d_'42''45'distrib'691''45''8851''45'nonPos_5346 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonPositive_154 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45'distrib'691''45''8851''45'nonPos_5346 = erased
-- Data.Rational.Properties.nonZero⇒1/nonZero
d_nonZero'8658'1'47'nonZero_5354 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_nonZero'8658'1'47'nonZero_5354 v0 ~v1
  = du_nonZero'8658'1'47'nonZero_5354 v0
du_nonZero'8658'1'47'nonZero_5354 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_nonZero'8658'1'47'nonZero_5354 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.1/-involutive
d_1'47''45'involutive_5360 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_1'47''45'involutive_5360 = erased
-- Data.Rational.Properties.1/pos⇒pos
d_1'47'pos'8658'pos_5374 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_1'47'pos'8658'pos_5374 v0 ~v1 = du_1'47'pos'8658'pos_5374 v0
du_1'47'pos'8658'pos_5374 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_1'47'pos'8658'pos_5374 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_Positive'46'constructor_1295
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Properties.1/neg⇒neg
d_1'47'neg'8658'neg_5380 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
d_1'47'neg'8658'neg_5380 v0 ~v1 = du_1'47'neg'8658'neg_5380 v0
du_1'47'neg'8658'neg_5380 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
du_1'47'neg'8658'neg_5380 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Integer.Base.C_Negative'46'constructor_1469
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Rational.Properties.pos⇒1/pos
d_pos'8658'1'47'pos_5388 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
d_pos'8658'1'47'pos_5388 v0 ~v1 ~v2 = du_pos'8658'1'47'pos_5388 v0
du_pos'8658'1'47'pos_5388 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134
du_pos'8658'1'47'pos_5388 v0
  = coe
      du_1'47'pos'8658'pos_5374
      (coe MAlonzo.Code.Data.Rational.Base.du_1'47'__282 (coe v0))
-- Data.Rational.Properties.neg⇒1/neg
d_neg'8658'1'47'neg_5398 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
d_neg'8658'1'47'neg_5398 v0 ~v1 ~v2 = du_neg'8658'1'47'neg_5398 v0
du_neg'8658'1'47'neg_5398 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164
du_neg'8658'1'47'neg_5398 v0
  = coe
      du_1'47'neg'8658'neg_5380
      (coe MAlonzo.Code.Data.Rational.Base.du_1'47'__282 (coe v0))
-- Data.Rational.Properties.toℚᵘ-homo-∣-∣
d_toℚ'7512''45'homo'45''8739''45''8739'_5402 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Unnormalised.Base.T__'8771'__24
d_toℚ'7512''45'homo'45''8739''45''8739'_5402 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Rational.Unnormalised.Base.C_'42''8801''42'_30)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.∣p∣≡0⇒p≡0
d_'8739'p'8739''8801'0'8658'p'8801'0_5406 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'p'8739''8801'0'8658'p'8801'0_5406 = erased
-- Data.Rational.Properties.0≤∣p∣
d_0'8804''8739'p'8739'_5412 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_0'8804''8739'p'8739'_5412 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2
        -> coe
             MAlonzo.Code.Data.Rational.Base.C_'42''8804''42'_52
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                (coe
                   MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                (\ v4 v5 v6 ->
                   coe
                     MAlonzo.Code.Data.Integer.Properties.du_'60''8658''8804'_2630 v6)
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                   (coe
                      MAlonzo.Code.Data.Rational.Base.d_numerator_14
                      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170))
                   (coe
                      MAlonzo.Code.Data.Rational.Base.d_denominator_22
                      (coe
                         MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                         (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2))))
                (coe
                   MAlonzo.Code.Data.Integer.Base.d__'42'__308
                   (coe
                      MAlonzo.Code.Data.Rational.Base.d_numerator_14
                      (coe
                         MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                         (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2)))
                   (coe
                      MAlonzo.Code.Data.Rational.Base.d_denominator_22
                      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170)))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                   (coe
                      MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                   (\ v4 v5 v6 v7 v8 ->
                      coe
                        MAlonzo.Code.Data.Integer.Properties.du_'8804''45''60''45'trans_2742
                        v7 v8)
                   (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12)
                   (coe
                      MAlonzo.Code.Data.Rational.Base.d_numerator_14
                      (coe
                         MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                         (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2)))
                   (coe
                      MAlonzo.Code.Data.Integer.Base.d__'42'__308
                      (coe
                         MAlonzo.Code.Data.Rational.Base.d_numerator_14
                         (coe
                            MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                            (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2)))
                      (coe
                         MAlonzo.Code.Data.Rational.Base.d_denominator_22
                         (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                      (coe
                         MAlonzo.Code.Data.Integer.Properties.d_'8804''45'isPreorder_2584)
                      (coe
                         MAlonzo.Code.Data.Integer.Base.d__'42'__308
                         (coe
                            MAlonzo.Code.Data.Rational.Base.d_numerator_14
                            (coe
                               MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                               (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2)))
                         (coe MAlonzo.Code.Data.Integer.Base.d_1ℤ_16)))
                   (coe
                      MAlonzo.Code.Data.Integer.Base.C_'43''8804''43'_48
                      (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.0≤p⇒∣p∣≡p
d_0'8804'p'8658''8739'p'8739''8801'p_5420 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'8804'p'8658''8739'p'8739''8801'p_5420 = erased
-- Data.Rational.Properties.∣-p∣≡∣p∣
d_'8739''45'p'8739''8801''8739'p'8739'_5428 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''45'p'8739''8801''8739'p'8739'_5428 = erased
-- Data.Rational.Properties.∣p∣≡p⇒0≤p
d_'8739'p'8739''8801'p'8658'0'8804'p_5442 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8739'p'8739''8801'p'8658'0'8804'p_5442 v0 ~v1
  = du_'8739'p'8739''8801'p'8658'0'8804'p_5442 v0
du_'8739'p'8739''8801'p'8658'0'8804'p_5442 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'8739'p'8739''8801'p'8658'0'8804'p_5442 v0
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204
      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170) (coe v0)
      (coe
         MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8739'p'8739''8771'p'8658'0'8804'p_3066
         (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'equality__190
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0)))
               (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                  (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
               (coe
                  MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'8771''45'sym_134
                  (coe d_toℚ'7512''45'homo'45''8739''45''8739'_5402 (coe v0))))))
-- Data.Rational.Properties.∣p∣≡p∨∣p∣≡-p
d_'8739'p'8739''8801'p'8744''8739'p'8739''8801''45'p_5454 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8739'p'8739''8801'p'8744''8739'p'8739''8801''45'p_5454 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2
        -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
             _ -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 erased
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.∣p+q∣≤∣p∣+∣q∣
d_'8739'p'43'q'8739''8804''8739'p'8739''43''8739'q'8739'_5468 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8739'p'43'q'8739''8804''8739'p'8739''43''8739'q'8739'_5468 v0 v1
  = coe
      d_toℚ'7512''45'cancel'45''8804'_3204
      (coe
         MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
         (coe
            MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Data.Rational.Base.d__'43'__260
         (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
         (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1)))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
         (\ v2 v3 v4 ->
            coe
              MAlonzo.Code.Data.Rational.Unnormalised.Properties.du_'60''8658''8804'_366
              v4)
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v1))))
         (coe
            MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
            (coe
               MAlonzo.Code.Data.Rational.Base.d__'43'__260
               (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
         (coe
            MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v1))))
            (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
               (coe
                  MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v1))))
            (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'43'__260
                  (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
            (coe
               MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771'_656
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v1))))
               (MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))))
               (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d__'43'__260
                     (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45''60''45'trans_402)
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                        (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))))
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'43'__260
                        (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                           (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0)))
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
                     (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d__'43'__260
                           (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
                     (coe
                        MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_step'45''8771''728'_658
                        (MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                              (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0)))
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                              (coe
                                 MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
                        (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d__'43'__260
                              (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                              (coe
                                 MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
                        (MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d__'43'__260
                              (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                              (coe
                                 MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                           (coe
                              MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8804''45'isPreorder_322)
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                              (coe
                                 MAlonzo.Code.Data.Rational.Base.d__'43'__260
                                 (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                                 (coe
                                    MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1)))))
                        (d_toℚ'7512''45'homo'45''43'_3706
                           (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0))
                           (coe
                              MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1))))
                     (MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'43''45'cong_766
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                           (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v0)))
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0)))
                        (coe
                           MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                           (coe MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328 (coe v1)))
                        (coe
                           MAlonzo.Code.Data.Rational.Unnormalised.Base.d_'8739'_'8739'_260
                           (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
                        (coe d_toℚ'7512''45'homo'45''8739''45''8739'_5402 (coe v0))
                        (coe d_toℚ'7512''45'homo'45''8739''45''8739'_5402 (coe v1))))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8739'p'43'q'8739''8804''8739'p'8739''43''8739'q'8739'_3096
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1))))
               (MAlonzo.Code.Data.Rational.Unnormalised.Properties.d_'8739''45''8739''45'cong_2962
                  (coe
                     MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158
                     (coe
                        MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v1)))
                  (coe
                     MAlonzo.Code.Data.Rational.Unnormalised.Base.d__'43'__196
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v0))
                     (coe MAlonzo.Code.Data.Rational.Base.d_toℚ'7512'_158 (coe v1)))
                  (coe d_toℚ'7512''45'homo'45''43'_3706 (coe v0) (coe v1))))
            (d_toℚ'7512''45'homo'45''8739''45''8739'_5402
               (coe
                  MAlonzo.Code.Data.Rational.Base.d__'43'__260 (coe v0) (coe v1)))))
-- Data.Rational.Properties.∣p-q∣≤∣p∣+∣q∣
d_'8739'p'45'q'8739''8804''8739'p'8739''43''8739'q'8739'_5482 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'8739'p'45'q'8739''8804''8739'p'8739''43''8739'q'8739'_5482 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
                    (coe d_'8804''45'isPreorder_3310)
                    (\ v8 v9 v10 -> coe du_'60''8658''8804'_3348 v10)
                    (coe
                       MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                       (coe
                          MAlonzo.Code.Data.Rational.Base.d__'45'__272
                          (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3)
                          (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6)))
                    (coe
                       MAlonzo.Code.Data.Rational.Base.d__'43'__260
                       (coe
                          MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                          (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3))
                       (coe
                          MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                          (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6)))
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
                       (coe d_'8804''45'isPreorder_3310)
                       (coe d_'8804''45''60''45'trans_3420)
                       (coe
                          MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d__'45'__272
                             (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3)
                             (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6)))
                       (coe
                          MAlonzo.Code.Data.Rational.Base.d__'43'__260
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                             (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3))
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                             (coe
                                MAlonzo.Code.Data.Rational.Base.d_'45'__104
                                (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6))))
                       (coe
                          MAlonzo.Code.Data.Rational.Base.d__'43'__260
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                             (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3))
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                             (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                          (coe d_'8804''45'isPreorder_3310)
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d__'43'__260
                             (coe
                                MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                                (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3))
                             (coe
                                MAlonzo.Code.Data.Rational.Base.d_'8739'_'8739'_328
                                (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6))))
                       (coe
                          d_'8739'p'43'q'8739''8804''8739'p'8739''43''8739'q'8739'_5468
                          (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v2 v3)
                          (coe
                             MAlonzo.Code.Data.Rational.Base.d_'45'__104
                             (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v5 v6))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.∣p*q∣≡∣p∣*∣q∣
d_'8739'p'42'q'8739''8801''8739'p'8739''42''8739'q'8739'_5498 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739'p'42'q'8739''8801''8739'p'8739''42''8739'q'8739'_5498
  = erased
-- Data.Rational.Properties.∣-∣-nonNeg
d_'8739''45''8739''45'nonNeg_5510 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_NonNegative_144
d_'8739''45''8739''45'nonNeg_5510 v0
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v1 v2
        -> coe
             seq (coe v1)
             (coe
                MAlonzo.Code.Data.Integer.Base.C_NonNegative'46'constructor_1353
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.∣∣p∣∣≡∣p∣
d_'8739''8739'p'8739''8739''8801''8739'p'8739'_5514 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8739''8739'p'8739''8739''8801''8739'p'8739'_5514 = erased
-- Data.Rational.Properties.*-monoʳ-≤-neg
d_'42''45'mono'691''45''8804''45'neg_5522 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'691''45''8804''45'neg_5522 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804''45'neg_5522 v0 v2 v3
du_'42''45'mono'691''45''8804''45'neg_5522 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'691''45''8804''45'neg_5522 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'mono'691''45''8804''45'nonPos_4496
                    (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4) (coe v1)
                    (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.*-monoˡ-≤-neg
d_'42''45'mono'737''45''8804''45'neg_5530 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'737''45''8804''45'neg_5530 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804''45'neg_5530 v0 v2 v3
du_'42''45'mono'737''45''8804''45'neg_5530 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'737''45''8804''45'neg_5530 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'mono'737''45''8804''45'nonPos_4516
                    (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4) (coe v1)
                    (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.*-monoʳ-≤-pos
d_'42''45'mono'691''45''8804''45'pos_5538 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'691''45''8804''45'pos_5538 v0 ~v1 v2 v3
  = du_'42''45'mono'691''45''8804''45'pos_5538 v0 v2 v3
du_'42''45'mono'691''45''8804''45'pos_5538 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'691''45''8804''45'pos_5538 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             0 -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'mono'691''45''8804''45'nonNeg_4454
                   (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4) (coe v1)
                   (coe v2)
             _ -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.*-monoˡ-≤-pos
d_'42''45'mono'737''45''8804''45'pos_5546 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
d_'42''45'mono'737''45''8804''45'pos_5546 v0 ~v1 v2 v3
  = du_'42''45'mono'737''45''8804''45'pos_5546 v0 v2 v3
du_'42''45'mono'737''45''8804''45'pos_5546 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46 ->
  MAlonzo.Code.Data.Rational.Base.T__'8804'__46
du_'42''45'mono'737''45''8804''45'pos_5546 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             0 -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'mono'737''45''8804''45'nonNeg_4474
                   (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4) (coe v1)
                   (coe v2)
             _ -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.*-cancelˡ-<-pos
d_'42''45'cancel'737''45''60''45'pos_5556 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'737''45''60''45'pos_5556 v0 ~v1 v2 v3
  = du_'42''45'cancel'737''45''60''45'pos_5556 v0 v2 v3
du_'42''45'cancel'737''45''60''45'pos_5556 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'737''45''60''45'pos_5556 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             0 -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'cancel'737''45''60''45'nonNeg_4620
                   (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4) (coe v1)
                   (coe v2)
             _ -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.*-cancelʳ-<-pos
d_'42''45'cancel'691''45''60''45'pos_5566 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'691''45''60''45'pos_5566 v0 ~v1 v2 v3
  = du_'42''45'cancel'691''45''60''45'pos_5566 v0 v2 v3
du_'42''45'cancel'691''45''60''45'pos_5566 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'691''45''60''45'pos_5566 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             0 -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ | coe geqInt (coe v3) (coe (1 :: Integer)) ->
                 coe
                   du_'42''45'cancel'691''45''60''45'nonNeg_4642
                   (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4) (coe v1)
                   (coe v2)
             _ -> coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.*-cancelˡ-<-neg
d_'42''45'cancel'737''45''60''45'neg_5576 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'737''45''60''45'neg_5576 v0 ~v1 v2 v3
  = du_'42''45'cancel'737''45''60''45'neg_5576 v0 v2 v3
du_'42''45'cancel'737''45''60''45'neg_5576 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'737''45''60''45'neg_5576 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'cancel'737''45''60''45'nonPos_4704 (coe v1) (coe v2)
                    (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.*-cancelʳ-<-neg
d_'42''45'cancel'691''45''60''45'neg_5586 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_'42''45'cancel'691''45''60''45'neg_5586 v0 ~v1 v2 v3
  = du_'42''45'cancel'691''45''60''45'neg_5586 v0 v2 v3
du_'42''45'cancel'691''45''60''45'neg_5586 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_'42''45'cancel'691''45''60''45'neg_5586 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4
        -> case coe v3 of
             _ | coe geqInt (coe v3) (coe (0 :: Integer)) ->
                 coe (\ v6 -> MAlonzo.RTE.mazUnreachableError)
             _ -> coe
                    du_'42''45'cancel'691''45''60''45'nonPos_4722 (coe v1) (coe v2)
                    (coe MAlonzo.Code.Data.Rational.Base.C_mkℚ_24 v3 v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Rational.Properties.negative<positive
d_negative'60'positive_5590 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Integer.Base.T_Negative_164 ->
  MAlonzo.Code.Data.Integer.Base.T_Positive_134 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
d_negative'60'positive_5590 v0 v1 ~v2 ~v3
  = du_negative'60'positive_5590 v0 v1
du_negative'60'positive_5590 ::
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T__'60'__54
du_negative'60'positive_5590 v0 v1
  = coe du_neg'60'pos_3628 (coe v0) (coe v1)
