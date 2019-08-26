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

(* Theorem we wish to show about rev and rev_append:
    forall xs, rev = qrev xs
 *)
(* DiscoverLemmas "QRevExample" rev qrev. *)

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
    
Definition myqrev {A : Type} (xs : mylist A) := rev_append xs nil.

(* DiscoverLemmas "QRevExample" rev myqrev.

Lemma lemma_1 : 
  forall (x : Type), 
  forall (y : (mylist x)),
  (rev y) = (myqrev y).
  Admitted. (* now coqhammer fails - bad at induction :( *)
Lemma lemma_2 : 
  forall (x : Type), 
  (myqrev (@nil x)) = (@nil x).
  hammer. Qed.
Lemma lemma_3 : 
  forall (x : Type), 
  forall (y : (mylist x)),
  (myqrev (myqrev y)) = y.
   Admitted. (* also fails this one *)
Lemma lemma_4 : 
  forall (x : Type), 
  forall (y : x),
  (myqrev (cons y (@nil x))) = (cons y (@nil x)).
  try hammer. Qed. *)

