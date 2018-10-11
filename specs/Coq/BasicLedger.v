Require Import Coq.Lists.List.
Require Import Coq.Logic.ClassicalFacts.

Set Implicit Arguments.

Generalizable All Variables.

Open Scope list_scope.

Axiom prf_prop_ex : prop_extensionality .

Axiom proof_irrelevance : forall (A : Prop) (a1 a2 : A), a1 = a2.

(* Definition decA (A : Type) := forall (x y : A), {x = y} + {x <> y}. *)

Definition Pow (A : Type) := list A.
Definition FinMap (A B : Type) := Pow (A * B).

(* Compute domain of FinMap *)
Fixpoint dom {A B : Type} (f : FinMap A B) : Pow A :=
  match f with
  | nil => nil
  | ((a, _) :: rest) => a :: dom rest
  end.

(* Compute range / codomain of FinMap *)
Fixpoint ran {A B : Type} (f : FinMap A B) : Pow B :=
  match f with
  | nil => nil
  | ((_, b) :: rest) => b :: ran rest
  end.

Check dom.

(* from Coq Spec *)
Class BEqType (A : Type) := {
        eqb : A -> A -> bool;
        eqb_leibniz : forall x y, eqb x y = true -> x = y  (* TODO: is this needed ?*)
      }.

(* Compute the domain restriction of f to U *)
Fixpoint dom_res {A : Type} `{A0 : BEqType A} {B : Type} (U : Pow A) (f : FinMap A B) : FinMap A B :=
  match f with
  | nil => nil
  | ((a, b) :: rest) => if (existsb (fun x => eqb a x) U)
                       then (a, b) :: (dom_res U rest)
                       else dom_res U rest
  end.

(* Compute the domain exclusion of U from f *)
Fixpoint dom_exclusion {A : Type} `{A0 : BEqType A} {B : Type} (U : Pow A) (f : FinMap A B) : FinMap A B :=
  match f with
  | nil => nil
  | ((a, b) :: rest) => if (existsb (fun x => eqb a x) U)
                       then dom_exclusion U rest
                       else (a, b) :: (dom_exclusion U rest)
  end.

(* Compute the codomain restriction of f to U *)
Fixpoint ran_res {A : Type} {B : Type} `{B0 : BEqType B} (f : FinMap A B) (U : Pow B) : FinMap A B :=
  match f with
  | nil => nil
  | ((a, b) :: rest) => if (existsb (fun x => eqb b x) U)
                       then (a, b) :: (ran_res rest U)
                       else ran_res rest U
  end.

(* Compute the codomain exclusion of U from f *)
Fixpoint ran_exclusion {A : Type} {B : Type} `{B0 : BEqType B} (f : FinMap A B) (U : Pow B) : FinMap A B :=
  match f with
  | nil => nil
  | ((a, b) :: rest) => if (existsb (fun x => eqb b x) U)
                       then ran_exclusion rest U
                       else (a, b) :: (ran_exclusion rest U)
  end.

Notation "U <| V" := (dom_res U V) (at level 50).
Notation "U <<| V" := (dom_exclusion U V) (at level 50).
Notation "V |> U" := (ran_res U V) (at level 50).
Notation "V |>> U" := (ran_exclusion U V) (at level 50).

Check ran_exclusion.

Lemma dom_res_nil_is_nil :
  forall (A B : Type) (A0 : BEqType A) (f : FinMap A B),
    nil <| f = nil.
Proof.
  intros A B A0 f.
  induction f.
  - tauto.
  - tauto.
Qed.

Lemma dom_exclusion_nil_is_idempotent :
  forall (A B : Type) (A0 : BEqType A) (f : FinMap A B),
    nil <<| f = f.
Proof.
  intros A B A0 f.
  induction f.
  - simpl; auto.
  - unfold dom_exclusion; simpl.
    unfold dom_exclusion in IHf.
    Admitted.

Lemma dom_first : forall (A B : Type) (a : A) (b : B) (f : FinMap A B),
    dom ((a, b) :: f) = a :: (dom f).
Proof.
  intros A B a b f.
  unfold dom.
  tauto.
Qed.

Lemma dom_res_subs_dom :
  forall (A B : Type) (A0 : BEqType A) (U : Pow A) (u : A) (f : FinMap A B),
    In u (dom (U <| f)) -> In u (dom f).
Proof.
  intros A B A0 U u f H.
  induction f.
  - simpl in H; case H.
  - destruct a as (a0, b).
    simpl dom.
    simpl.
    unfold dom_res in H.
    destruct existsb in H.
    + simpl in H.
      case H.
      * intros H0.
        apply or_introl.
        exact H0.
      * intro.
        apply or_intror.
        apply IHf.
        tauto.
    + apply or_intror.
      tauto.
Qed.

Lemma dom_exclusion_subs_dom :
  forall (A B : Type) (A0 : BEqType A) (U : Pow A) (u : A) (f : FinMap A B),
    In u (dom (U <<| f)) -> In u (dom f).
Proof.
  intros A B A0 U u f H.
  induction f.
  - simpl in H; case H.
  - destruct a as (a0, b).
    simpl.
    unfold dom_exclusion in H.
    destruct existsb in H.
    + apply or_intror.
      tauto.
    + simpl in H.
      case H.
      * intro; apply or_introl; assumption.
      * intro; apply or_intror.
        apply IHf; tauto.
Qed.

Check existsb.

Lemma dom_res_sub_f :
  forall (A B : Type) (A0 : BEqType A) (U : Pow A) (f : FinMap A B),
    incl (U <| f) f.
Proof.
  intros A B A0 U f.
  induction f.
  - simpl. unfold incl; intros; assumption.
  - simpl.
    compute.
    intros.

Lemma dom_res_sub_f :
  forall (A B : Type) (A0 : BEqType A) (U : Pow A) (a : A) (b : B) (f : FinMap A B),
    In (a, b) (U <| f) -> In (a, b) f.
Proof.
  intros A B A0 U a b f H.
  induction f.
  - simpl in H; case H.
  - simpl.
    unfold dom_res in H.
    destruct (existsb (eqb a)).
    + exact U.
