(** * Library file that should be used for the term projects of introduction
to functional programming 2015. This file is based on SfLib.v of Software
Foundations by Benjamin Pierce and others. *)

(** * From the Coq Standard Library *)

Require Export Bool.
Require Export List.
Export ListNotations.
Require Export Arith.
Require Export NPeano.
Require Export Arith.EqNat.  (* Contains [beq_nat], among other things *)


(** * From Basics.v *)

Require String. Open Scope string_scope.
From Coq Require Import Extraction.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.
Add LoadPath "~/Documents/cpdt/src" as Cpdt.
Require Import Cpdt.CpdtTactics.


Extract Inductive bool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inductive option => "Prelude.Maybe" [ "Prelude.Just" "Prelude.Nothing" ].
Extract Inductive unit => "()" [ "()" ].
Extract Inductive list => "([])" [ "([])" "(:)" ].
Extract Inductive prod => "(,)" [ "(,)" ].

Extract Inductive sumbool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inductive sumor => "Prelude.Maybe" [ "Prelude.Just" "Prelude.Nothing" ].
Extract Inductive sum => "Prelude.Either" [ "Prelude.Left" "Prelude.Right" ].

Extract Inlined Constant andb => "(Prelude.&&)".
Extract Inlined Constant orb => "(Prelude.||)".
(* Extract Inlined Constant negb => "Prelude.not". *)

(** * From Logic.v *)
(**  Identifiers *)

(** We define a new inductive datatype [Id] so that we won't confuse
    identifiers and numbers. *)

Inductive id : Type :=
  Id : nat -> id.

Definition beq_id (x x' : id) : bool :=
  match x, x' with
  | Id y, Id y' => beq_nat y y'
  end.
Lemma beq_id_refl : forall x : id,
  true = beq_id x x.
Proof.
  intros x; destruct x as [y]; simpl; apply beq_nat_refl.
Qed.
Lemma beq_id_eq: forall x x' : id,
  true = beq_id x x' -> x = x'.
Proof.
  intros x x' H; destruct x as [y]; destruct x' as [y']; simpl in H.
  apply beq_nat_eq in H. rewrite H. reflexivity.
Qed.

(* Exercise 2.1 *)
Inductive form :=
  | var : id -> form
  | ftrue : form
  | ffalse : form
  | and : form -> form -> form
  | or : form -> form -> form
  | imp : form -> form -> form
  | neg : form -> form.


Bind Scope form_scope with form.
Infix "/\" := and : form_scope.
Infix "\/" := or : form_scope.
Infix "-->" := imp (at level 70) : form_scope.
Notation "'!' b" := (neg b) (at level 60) : form_scope.
Notation "'vId' b" := (var (Id b)) (at level 55) : form_scope.
Open Scope form_scope.

Compute (or ffalse ftrue).
Compute (ffalse \/ ftrue).

(* Exercise 2.2 *)
Definition X := vId 0.
Definition Y := vId 1.
Definition Z := vId 2.

Definition ex1 : form := (X \/ !Y) /\ (!X \/ Y).
(* to test that the introduced notation behaves correctly: *)
Example ex1_correct : ex1 = (and (or X (neg Y)) (or (neg X) Y)).
Proof. reflexivity. Qed.

Definition ex2 : form := !Y --> (X \/ Y).
Example ex2_correct : ex2 = imp (neg Y) (or X Y).
Proof. reflexivity. Qed.
 
(* Note that /\ and \/ are right-associative *)
Definition ex3 : form := X /\ !X /\ ftrue.
Example ex3_correct : ex3 = and X (and (neg X) ftrue).
Proof. unfold ex3. reflexivity. Qed.

Definition valuation := id -> bool.
Definition empty_valuation : valuation := fun x => false.
Definition override (V : valuation) (x : id) (b : bool) : valuation :=
  fun y => if  beq_id x y then b else V y.

(* Exercise 2.3 *)
Fixpoint  interp (V : valuation) (p : form) : bool :=
  match p with
  | ffalse => false
  | ftrue => true
  | var id => V id
  | and f1 f2 => (interp V f1) && (interp V f2)
  | or f1 f2 => (interp V f1) || (interp V f2)
  | imp f1 f2 => if (interp V f1) then (interp V f2) else true
  | neg f => negb (interp V f)
  end.


Definition  satisfiable (p : form) : Prop :=
  exists V : valuation , interp V p = true.

(* Exercise 2.4 *)
Lemma  test1 : satisfiable  ex1.
Proof.
  unfold satisfiable. unfold ex1.
  (* we choose X = Y = false *)
  exists empty_valuation. 
  simpl. reflexivity.
Qed.

Lemma  test2 : satisfiable ex2.
Proof.
  unfold satisfiable. unfold ex2.
  (* we choose X = true, Y = false *)
  exists (override empty_valuation (Id 0) (true)). 
  simpl. reflexivity.
Qed.



(* Exercise 2.5: find_valuation *)

(* first some helper functions and representations... *)

Fixpoint contains_id (i : id) (ids : list id) : bool :=
  match ids with
  | x :: xs =>  if beq_id x i then true else (contains_id i xs)
  | [] => false
  end.

(* takes two lists of ids and returns a a merged list with no duplicates *)
Fixpoint merge_id_lists (l1 l2 : list id) : list id :=
  match l1 with
  | x :: xs => if contains_id x l2 
    then (merge_id_lists xs l2)
    else (merge_id_lists xs (x :: l2))
  | [] => l2
 end.

Fixpoint form_ids_aux (p : form) (ids : list id) : list id :=
  match p with
  | ffalse => ids
  | ftrue => ids
  | var id => if (contains_id id ids) then ids else id :: ids
  | neg f => form_ids_aux f ids
  | and f1 f2 => 
    merge_id_lists (form_ids_aux f1 ids) (form_ids_aux f2 ids)
  | or f1 f2 =>
    merge_id_lists (form_ids_aux f1 ids) (form_ids_aux f2 ids)
  | imp f1 f2 => 
    merge_id_lists (form_ids_aux f1 ids) (form_ids_aux f2 ids)
  end.

Definition form_ids (p : form) : list id := form_ids_aux p [].
Definition assignment := list (id * bool).
Definition empty_assignment (p : form) : assignment :=
  map (fun id => (id, false)) (form_ids p).

(* 
  this function essentially models binary (big-endian) increment
  on the second element of the tuples.
*)
Fixpoint next_assignment (a : assignment) : assignment :=
  match a with
  | [] => []
  | (id, b) :: xs => if b
    then (id, false) :: next_assignment xs
    else (id, true) :: xs
  end. 

Example next_assignment_ex : next_assignment [(Id 1,true);(Id 2,true);(Id 3,false)] = [(Id 1,false);(Id 2,false);(Id 3,true)].
Proof.
(* input represents 110 (3 in decimal). output represents 001 (4 in decimal) *)
reflexivity.
Qed.

Definition is_all_true_assignment (a : assignment) : bool :=
  forallb (fun idb => match idb with (id, b) => b end) a.  

Definition assignment_to_valuation (a : assignment) : valuation :=
  let f := fun i => (fun x => match x with (id,v) => beq_id i id end)
  in fun (i : id) => match find (f i) a with
    | Some (_,v) => v 
    | None => false
    end.

(* check if the given assignment satisfies p. 
   If not, try the next assignment. Stop when assignment is all true *)
Fixpoint find_valuation_aux 
  (p : form) (a : assignment) (fuel : nat) : option valuation :=
  let V' := assignment_to_valuation a in
  match fuel with
  | 0 => if interp V' p 
      then Some V'
      else None
  | S n => if interp V' p
    then Some V'
    else find_valuation_aux p (next_assignment a) n
  end.

Definition find_valuation (p : form) : option valuation :=
  let fuel := (2^(length (form_ids p))) in
  find_valuation_aux p (empty_assignment p) fuel.

Definition solver (p : form) : bool :=
  match find_valuation p with
  | Some _ => true
  | None => false
  end.

(* Exercise 2.6 Explain the difference between satisfiable and solver *)
(* 
    Satisfiable is a proposition on formulae that describes when a formula is true.
    Solver is a function that essentially decides if the satisfiable property is true or
    not for a given formula.
*)

(* Exercise 2.7 Write 2 positive and 2 negative tests of the solver and prove these tests using the reflexivity tactic *)

Example pos_ex1 : solver (X /\ Y --> Y) = true.
Proof.
(* X = Y = true =>  false -> false, which is true *)
reflexivity.
Qed.

Example pos_ex2 : solver (Y --> !(Y \/ X --> (ftrue /\ ffalse))) = true.
Proof.
(* X = Y = true =>  false -> false, which is true *)
reflexivity.
Qed.

Example neg_ex3 : solver (X /\ !X \/ (ftrue --> ffalse)) = false.
Proof.
reflexivity.
Qed.

Example neg_ex4 : solver (X /\ !X) = false.
Proof.
reflexivity.
Qed.

Lemma find_valuation_aux_sound : forall p A V n,
  find_valuation_aux p A n = Some V -> satisfiable p.
Proof.
  intros. unfold satisfiable. generalize dependent A.
  induction n ; intros ; try unfold find_valuation_aux in H.
  - destruct (interp (assignment_to_valuation A) p) eqn:H1.
    + exists (assignment_to_valuation A).
      apply H1.
    + inversion H.
  - destruct (interp (assignment_to_valuation A) p) eqn:H1.
    + exists (assignment_to_valuation A).
      apply H1.
    + apply (IHn (next_assignment A)). apply H.
Qed.
  
Lemma find_valuation_sound : forall p V,
  find_valuation p = Some V -> satisfiable p.
Proof. 
  unfold find_valuation. intros. 
  apply (find_valuation_aux_sound p (empty_assignment p) V (2 ^ length (form_ids p))).
  apply H.
Qed.

(* Exercise 2.8 *)
Theorem  solver_sound : forall p,
  solver p = true -> satisfiable p.
Proof.
  unfold solver. intros.
  destruct (find_valuation p) eqn:H1.
  - apply (find_valuation_sound p v).
    apply H1.
  - inversion H. 
Qed.

(* Exercise 2.9: Negational Normal Form *)

Fixpoint nnf_aux (p : form) (fuel : nat) : form :=
  match fuel with
  | 0 => p
  | S n => match p with
    | ! ffalse => ftrue
    | ! ftrue => ffalse
    | ! var id => p
    | ! (f1 /\ f2) => (* de morgans law 1: ~(a /\ b) <=> ~a \/ ~b *)
      (nnf_aux (neg f1) n) 
      \/ (nnf_aux (neg f2) n)
    | ! (f1 \/ f2) => (* de morgans law 2: ~(a \/ b) <=> ~a /\ ~b *)
      (nnf_aux (neg f1) n) 
      /\ (nnf_aux (neg f2) n)
    | ! (f1 --> f2) => (* ~(f1 -> f2) <=> ~(~f1 \/ f2) <=> (f1 /\ ~f2 *)
      (nnf_aux f1 n) /\ (nnf_aux (neg f2) n)
    | !!f => nnf_aux f n  
    | f1 /\ f2 => 
      (nnf_aux f1 n) /\ (nnf_aux f2 n)
    | f1 \/ f2 => 
      (nnf_aux f1 n) \/ (nnf_aux f2 n)
    | f1 --> f2 => 
      (nnf_aux f1 n) --> (nnf_aux f2 n)
    | _ => p
    end
  end.


(* helper function to determine how much "gas" to use in the function 
  nnf_aux_aux *)
Fixpoint size_of_form (p : form) : nat :=
  match p with
  | ffalse => 1
  | ftrue => 1
  | var id => 1
  | f1 /\ f2 => (size_of_form f1) + (size_of_form f2)
  | f1 \/ f2 => (size_of_form f1) + (size_of_form f2)
  | f1 --> f2 => (size_of_form f1) + (size_of_form f2)
  | !f => S (size_of_form f)
  end.  

(* computes the negational normal form of a boolean formula *)
Definition nnf (p : form) : form := nnf_aux p (2 * size_of_form p).
Definition nnf_solver (p : form) : bool := solver (nnf p).

Definition neg_interp (V : valuation) (p : form) := negb (interp V p).

MaxTermSize 6.
(* DiscoverLemmas "treesTest" nnf nnf_solver solver find_valuation interp  nnf.    *)

(* 

The file treesTest.hs has been created by extraction.
executing command: tip-spec --prune --size 5 --test-size 14 -i interp -i find_valuation -i solver -i nnf_solver -i nnf treesTest.smt2 > qs_res_treesTest.smt2
Lemma lemma_1 : 
  forall (x : (nat) -> Bool) (y : form),
  (is_true (interp x y)) -> (is_true (nnf_solver y)).
  Proof. Admitted.
Lemma lemma_2 : 
  forall (x : form),
  (is_true (solver x)) -> (is_true (nnf_solver x)).
  Proof. Admitted.
Lemma lemma_3 : 
  forall (x : form),
  (nnf_solver x) = (solver x).
  Proof. Admitted.
Lemma lemma_4 : 
  (nnf ffalse) = ffalse.
  Proof. Admitted.
Lemma lemma_5 : 
  (nnf ftrue) = ftrue.
  Proof. Admitted.
Lemma lemma_6 : 
  false = ((nnf_solver ffalse)).
  Proof. Admitted.
Lemma lemma_7 : 
  (nnf_solver ftrue).
  Proof. Admitted.
Lemma lemma_8 : 
  forall (x : (nat) -> Bool),
  false = (interp x ffalse).
  Proof. Admitted.
Lemma lemma_9 : 
  forall (x : (nat) -> Bool),
  is_true (interp x ftrue).
  Proof. Admitted.
Lemma lemma_10 : 
  forall (x : nat),
  (nnf (var x)) = (var x).
  Proof. Admitted.
Lemma lemma_11 : 
  forall (x : form),
  (nnf (nnf x)) = (nnf x).
  Proof. Admitted.
Lemma lemma_12 : 
  forall (x : nat),
  is_true (nnf_solver (var x)).
  Proof. Admitted.
Lemma lemma_13 : 
  forall (x : form),
  (nnf_solver (nnf x)) = (nnf_solver x).
  Proof. Admitted.
Lemma lemma_14 : 
  (nnf (neg ffalse)) = ftrue.
  Proof. Admitted.
Lemma lemma_15 : 
  (nnf (neg ftrue)) = ffalse.
  Proof. Admitted.
Lemma lemma_16 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (nnf y)) = (interp x y).
  Proof. Admitted.
Lemma lemma_17 : 
  forall (x : form) (y : form),
  (nnf_solver (and y x)) = (nnf_solver (and x y)).
  Proof. Admitted.
Lemma lemma_18 : 
  forall (x : form),
  (nnf_solver (and x x)) = (nnf_solver x).
  Proof. Admitted.
Lemma lemma_19 : 
  forall (x : form),
  is_true (nnf_solver (imp x x)).
  Proof. Admitted.
Lemma lemma_20 : 
  forall (x : (nat) -> Bool) (y : form) (z : form),
  (is_true (interp x z)) -> (is_true (nnf_solver (imp y z))).
  Proof. Admitted.
Lemma lemma_21 : 
  forall (x : form) (y : form),
  (is_true (nnf_solver y)) -> (is_true (nnf_solver (imp x y))).
  Proof. Admitted.
Lemma lemma_22 : 
  forall (x : form) (y : form),
  (nnf_solver (or y x)) = (nnf_solver (or x y)).
  Proof. Admitted.
Lemma lemma_23 : 
  forall (x : form),
  (nnf_solver (or x x)) = (nnf_solver x).
  Proof. Admitted.
Lemma lemma_24 : 
  forall (x : (nat) -> Bool) (y : form) (z : form),
  (is_true (interp x z)) -> (is_true (nnf_solver (or y z))).
  Proof. Admitted.
Lemma lemma_25 : 
  forall (x : form) (y : form),
  (is_true (nnf_solver y)) -> (is_true (nnf_solver (or x y))).
  Proof. Admitted.
Lemma lemma_26 : 
  forall (x : form),
  (nnf (and x ffalse)) = (and (nnf x) ffalse).
  Proof. Admitted.
Lemma lemma_27 : 
  forall (x : form),
  (nnf (and x ftrue)) = (and (nnf x) ftrue).
  Proof. Admitted.
Lemma lemma_28 : 
  forall (x : form),
  (nnf (and ffalse x)) = (and ffalse (nnf x)).
  Proof. Admitted.
Lemma lemma_29 : 
  forall (x : form),
  (nnf (and ftrue x)) = (and ftrue (nnf x)).
  Proof. Admitted.
Lemma lemma_30 : 
  forall (x : form),
  (nnf (imp x ffalse)) = (imp (nnf x) ffalse).
  Proof. Admitted.
Lemma lemma_31 : 
  forall (x : form),
  (nnf (imp x ftrue)) = (imp (nnf x) ftrue).
  Proof. Admitted.
Lemma lemma_32 : 
  forall (x : form),
  (nnf (imp ffalse x)) = (imp ffalse (nnf x)).
  Proof. Admitted.
Lemma lemma_33 : 
  forall (x : form),
  (nnf (imp ftrue x)) = (imp ftrue (nnf x)).
  Proof. Admitted.
Lemma lemma_34 : 
  forall (x : form),
  (nnf (neg (neg x))) = (nnf x).
  Proof. Admitted.
Lemma lemma_35 : 
  forall (x : nat),
  (nnf (neg (var x))) = (neg (var x)).
  Proof. Admitted.
Lemma lemma_36 : 
  forall (x : form),
  (nnf (or x ffalse)) = (or (nnf x) ffalse).
  Proof. Admitted.
Lemma lemma_37 : 
  forall (x : form),
  (nnf (or x ftrue)) = (or (nnf x) ftrue).
  Proof. Admitted.
Lemma lemma_38 : 
  forall (x : form),
  (nnf (or ffalse x)) = (or ffalse (nnf x)).
  Proof. Admitted.
Lemma lemma_39 : 
  forall (x : form),
  (nnf (or ftrue x)) = (or ftrue (nnf x)).
  Proof. Admitted.
Lemma lemma_40 : 
  forall (x : form),
  false = (nnf_solver (and x ffalse)).
  Proof. Admitted.
Lemma lemma_41 : 
  forall (x : form),
  (nnf_solver (and x ftrue)) = (nnf_solver x).
  Proof. Admitted.
Lemma lemma_42 : 
  forall (x : form),
  (nnf_solver (imp x ffalse)) = (nnf_solver (neg x)).
  Proof. Admitted.
Lemma lemma_43 : 
  forall (x : form),
  is_true (nnf_solver (imp ffalse x)).
  Proof. Admitted.
Lemma lemma_44 : 
  forall (x : form),
  (nnf_solver (imp ftrue x)) = (nnf_solver x).
  Proof. Admitted.
Lemma lemma_45 : 
  forall (x : nat),
  is_true (nnf_solver (neg (var x))).
  Proof. Admitted.
Lemma lemma_46 : 
  forall (x : form),
  (nnf_solver (or x ffalse)) = (nnf_solver x).
  Proof. Admitted.
Lemma lemma_47 : 
  forall (x : (nat) -> Bool) (y : form),
  (is_true (interp x y)) -> (is_true (negb (interp x (neg y)))).
  Proof. Admitted.
Lemma lemma_48 : 
  forall (x : form) (y : (nat) -> Bool) (z : nat),
  (is_true (interp y x)) -> (is_true (interp y (var z)) = ( y z)).
  Proof. Admitted.
Lemma lemma_49 : 
  forall (x : (nat) -> Bool) (y : form) (z : form),
  (interp x (and z y)) = (interp x (and y z)).
  Proof. Admitted.
Lemma lemma_50 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (and y y)) = (interp x y).
  Proof. Admitted.
Lemma lemma_51 : 
  forall (x : (nat) -> Bool) (y : form),
  is_true (interp x (imp y y)).
  Proof. Admitted.
Lemma lemma_52 : 
  forall (x : (nat) -> Bool) (y : form) (z : form),
  (interp x (or z y)) = (interp x (or y z)).
  Proof. Admitted.
Lemma lemma_53 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (or y y)) = (interp x y).
  Proof. Admitted.
Lemma lemma_54 : 
  forall (x : form) (y : form),
  (and (nnf x) (nnf y)) = (nnf (and x y)).
  Proof. Admitted.
Lemma lemma_55 : 
  forall (x : form) (y : form),
  (imp (nnf x) (nnf y)) = (nnf (imp x y)).
  Proof. Admitted.
Lemma lemma_56 : 
  forall (x : form) (y : form),
  (or (nnf x) (nnf y)) = (nnf (or x y)).
  Proof. Admitted.
Lemma lemma_57 : 
  forall (x : (nat) -> Bool) (y : form),
  false = (interp x (and y ffalse)).
  Proof. Admitted.
Lemma lemma_58 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (and y ftrue)) = (interp x y).
  Proof. Admitted.
Lemma lemma_59 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (imp y ffalse)) = (interp x (neg y)).
  Proof. Admitted.
Lemma lemma_60 : 
  forall (x : (nat) -> Bool) (y : form),
  is_true (interp x (imp y ftrue)).
  Proof. Admitted.
Lemma lemma_61 : 
  forall (x : (nat) -> Bool) (y : form),
  is_true (interp x (imp ffalse y)).
  Proof. Admitted.
Lemma lemma_62 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (imp ftrue y)) = (interp x y).
  Proof. Admitted.
Lemma lemma_63 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (neg (nnf y))) = (interp x (neg y)).
  Proof. Admitted.
Lemma lemma_64 : 
  forall (x : (nat) -> Bool) (y : form),
  (interp x (or y ffalse)) = (interp x y).
  Proof. Admitted.
Lemma lemma_65 : 
  forall (x : (nat) -> Bool) (y : form),
  is_true (interp x (or y ftrue)).
  Proof. Admitted.
Lemma lemma_66 : 
  forall (x : form) (y : form),
  (nnf (neg (imp x y))) = (nnf (and x (neg y))).
  Proof. Admitted.
Lemma lemma_67 : 
  forall (x : form),
  false = (nnf_solver (and x (neg x))).
  Proof. Admitted.
Lemma lemma_68 : 
  forall (x : form) (y : form),
  (nnf_solver (imp y (neg x))) = (nnf_solver (imp x (neg y))).
  Proof. Admitted.
Lemma lemma_69 : 
  forall (x : form),
  (nnf_solver (imp x (neg x))) = (nnf_solver (neg x)).
  Proof. Admitted.
Lemma lemma_70 : 
  forall (x : form) (y : form),
  (nnf_solver (imp (neg x) y)) = (nnf_solver (or x y)).
  Proof. Admitted.
Lemma lemma_71 : 
  forall (x : nat) (y : form),
  is_true (nnf_solver (imp (var x) y)).
  Proof. Admitted.
Lemma lemma_72 : 
  forall (x : form) (y : form),
  (nnf_solver (neg (and x y))) = (nnf_solver (imp x (neg y))).
  Proof. Admitted.
Lemma lemma_73 : 
  forall (x : form) (y : form),
  (nnf_solver (neg (or y x))) = (nnf_solver (neg (or x y))).
  Proof. Admitted.
Lemma lemma_74 : 
  forall (x : form),
  (nnf_solver (neg (or x x))) = (nnf_solver (neg x)).
  Proof. Admitted.
Lemma lemma_75 : 
  forall (x : form) (y : form),
  (nnf_solver (or y (neg x))) = (nnf_solver (imp x y)).
  Proof. Admitted.
Lemma lemma_76 : 
  forall (x : form),
  (nnf (neg (and x ffalse))) = (or (nnf (neg x)) ftrue).
  Proof. Admitted.
Lemma lemma_77 : 
  forall (x : form),
  (nnf (neg (and x ftrue))) = (or (nnf (neg x)) ffalse).
  Proof. Admitted.
Lemma lemma_78 : 
  forall (x : form),
  (nnf (neg (and ffalse x))) = (or ftrue (nnf (neg x))).
  Proof. Admitted.
Lemma lemma_79 : 
  forall (x : form),
  (nnf (neg (and ftrue x))) = (or ffalse (nnf (neg x))).
  Proof. Admitted.
Lemma lemma_80 : 
  forall (x : form),
  (nnf (neg (or x ffalse))) = (and (nnf (neg x)) ftrue).
  Proof. Admitted.
Lemma lemma_81 : 
  forall (x : form),
  (nnf (neg (or x ftrue))) = (and (nnf (neg x)) ffalse).
  Proof. Admitted.
Lemma lemma_82 : 
  forall (x : form),
  (nnf (neg (or ffalse x))) = (and ftrue (nnf (neg x))).
  Proof. Admitted.
Lemma lemma_83 : 
  forall (x : form),
  (nnf (neg (or ftrue x))) = (and ffalse (nnf (neg x))).
  Proof. Admitted.



 *)
Close Scope form_scope.