From Coq Require Import Extraction.
From Coq Require Import List.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.
Add LoadPath "~/Documents/cpdt/src" as Cpdt.
Require Import Cpdt.CpdtTactics.

Open Scope list_scope.


Print rev_append.

(* important to declare A as implicit because extraction to haskell
   removes any argument of kind 'Type' *)
Definition qrev {A : Type} (xs : list A) := rev_append xs nil.

Print qrev.

  
Fixpoint myqrev {A : Type} (l l': list A) : list A :=
    match l with
      | nil => l'
      | a::l => myqrev l (a::l')
    end.

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
Extract Inlined Constant negb => "(Prelude.not)".
Extract Inlined Constant app => "(Prelude.++)".

(* DiscoverLemmas "QRevExample" rev myqrev app. *)

(* Lemma lemma_4 : 
  forall (x : Type), 
  forall (y : (list x)),
  (myqrev y (@nil x)) = (rev y).
  Proof. crush. induction y; try simpl; try crush. Show Proof. Admitted.

Lemma lemma_7 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (app (rev y) z) = (myqrev y z).
  Proof. crush. induction y; try simpl; try crush. hammer.
 *)  



(* Theorem we wish to show about rev and rev_append:
    forall xs, rev = qrev xs
 *)


(* Lemma lemma_1 : 
  forall (x : Type), 
  forall (y : (list x)),
  (rev y) = (qrev y).
  hammer. Qed.
(*   coqhammer used Coq.Lists.List.rev_alt to prove this, so we don't really know how good it is at performing induction itself... *)
Lemma lemma_2 : 
  forall (x : Type), 
  (qrev (@nil x)) = (@nil x).
  hammer. Qed.
Lemma lemma_3 : 
  forall (x : Type), 
  forall (y : (list x)),
  (qrev (qrev y)) = y.
  hammer. Qed.
Lemma lemma_4 : 
  forall (x : Type), 
  forall (y : x),
  (qrev (cons y (@nil x))) = (cons y (@nil x)).
  hammer. Qed. *)

Close Scope list_scope.

Set Warnings "-notation-overridden,-parsing".
Inductive mylist (A : Type) : Type :=
 | nil : mylist A
 | cons : A -> mylist A -> mylist A.
Arguments nil {X} : rename.
Arguments cons {X} _ _ : rename.
Infix "::" := cons (at level 60, right associativity).

Definition app {A : Type} : mylist A -> mylist A -> mylist A :=
  fix app l m :=
  match l with
   | nil => m
   | cons a l1 => cons a (app l1 m)
  end.

Fixpoint rev {A : Type} (l : mylist A) : mylist A :=
  match l with
    | nil => nil
    | cons x l' => app (rev l') (cons x nil)
  end.
  
Fixpoint rev_append {A : Type} (l l': mylist A) : mylist A :=
    match l with
      | nil => l'
      | a::l => rev_append l (a::l')
    end.
    



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
Extract Inlined Constant negb => "(Prelude.not)".
Extract Inlined Constant app => "(Prelude.++)".


(* DiscoverLemmas "QRevExample" rev rev_append app.
Lemma lemma_1 : 
  forall (x : Type), 
  (rev (@nil x)) = (@nil x).
  Proof. Admitted.
Lemma lemma_2 : 
  forall (x : Type), 
  forall (y : (mylist x)),
  (app y (@nil x)) = y.
  Proof. Admitted.
Lemma lemma_3 : 
  forall (x : Type), 
  forall (y : (mylist x)),
  (app (@nil x) y) = y.
  Proof. Admitted.
Lemma lemma_4 : 
  forall (x : Type), 
  forall (y : (mylist x)),
  (rev (rev y)) = y.
  Proof. Admitted.
Lemma lemma_5 : 
  forall (x : Type), 
  forall (y : (mylist x)),
  (rev_append y (@nil x)) = (rev y).
  Proof. Admitted.
Lemma lemma_6 : 
  forall (x : Type), 
  forall (y : (mylist x)),
  (rev_append (@nil x) y) = y.
  Proof. Admitted.
Lemma lemma_7 : 
  forall (x : Type), 
  forall (y : (mylist x)) (z : (mylist x)),
  (app (rev y) z) = (rev_append y z).
  Proof. crush. induction y; try simpl; try crush.  hammer. Admitted.
Lemma lemma_8 : 
  forall (x : Type), 
  forall (y : (mylist x)) (z : (mylist x)),
  (rev (rev_append z y)) = (rev_append y z).
  Proof. Admitted.
Lemma lemma_9 : 
  forall (x : Type), 
  forall (y : x),
  (rev (cons y (@nil x))) = (cons y (@nil x)).
  Proof. Admitted.
Lemma lemma_10 : 
  forall (x : Type), 
  forall (y : x) (z : (mylist x)) (x2 : (mylist x)),
  (app (cons y z) x2) = (cons y (app z x2)).
  Proof. Admitted.
Lemma lemma_11 : 
  forall (x : Type), 
  forall (y : (mylist x)) (z : (mylist x)) (x2 : (mylist x)),
  (app (app y z) x2) = (app y (app z x2)).
  Proof. Admitted.
Lemma lemma_12 : 
  forall (x : Type), 
  forall (y : (mylist x)) (z : x) (x2 : (mylist x)),
  (rev_append (cons z y) x2) = (rev_append y (cons z x2)).
  Proof. Admitted.
 *)
