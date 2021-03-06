From Coq Require Import Extraction.
From Coq Require Import List.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.
Add LoadPath "~/Documents/cpdt/src" as Cpdt.
Require Import Cpdt.CpdtTactics.
Open Scope list_scope.


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
Extract Inlined Constant negb => "Prelude.not".

MaxTermSize 7.
MaxTestSize 20.

Definition retOpt {A : Type} (x : A) := Some x.
 
Definition bindOpt {A B : Type} (a : option A) (f : A -> option B) : option B :=
  match a with
    | Some x => f x
    | None => None
  end.

Print map.
(* Definition fmap {A B : Type} (f : A -> B) (l : list A) :=
  match l with
  | [] => []
  | x :: xs => (f x)  *)
  
Import ListNotations.

Fixpoint join {A : Type} (l : list (list A)) :=
  match l with
  | [] => []
  | xs :: xss => xs ++ (join xss)
  end.

Definition ret {A : Type} (x : A) := [x].

Definition bind {A B : Type} (l : list A) (f : A -> list B) : list B :=
  flat_map f l.
  
Notation "A >>= F" := (bind A F) (at level 42, left associativity).

(* Lemma list_mon_assoc :
  forall (A B C : Type) (a : list A) (f : A -> list B) (g : B -> list C),
    (a >>= f) >>= g = a >>= (fun x => f x >>= g).
intros.
induction a; repeat reflexivity. 
unfold ">>=". 
simpl. unfold ">>=" in IHa.
rewrite map_app.
rewrite <- IHa.
rewrite concat_app. reflexivity.
Qed. *)

Notation "'do' X <- A ; B" := (bind A (fun X => B)) (at level 200, X ident, A at level 100, B at level 200).

Definition kleisli_comp {A B C : Type} (f : A -> list B) (g : B -> list C) : (A -> list C) := fun a => do  
    y <- f a;
    g y.
(* equivalently: bind (f a) (fun y => g y) *) 

Definition compose {A B C : Type} (f : A -> B) (g : C -> A) :=
  fun x => f (g x).


(* DiscoverLemmas "ListMonadTest" bind ret kleisli_comp.   *)

(* Lemma asd : forall (A B : Type) (a : A) (f : A -> list B),
  bind (ret a) f = f a.
Proof. crush. Qed. 

Lemma lemma_1 : 
  forall (x : Type), 
  forall (y : x),
  (cons y (@nil x)) = (ret y).
  Proof. crush. Qed.
Lemma lemma_2 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (x) -> (list y)),
  (bind (@nil x) z) = (@nil y).
  Proof. crush. Qed.
Lemma lemma_3 : 
  forall (x : Type), 
  forall (y : (list x)),
  (bind y (fun (z : x)  =>  (ret z))) = y.
  Proof. crush. Admitted.
Lemma lemma_4 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (x) -> (list y)) (x2 : x),
  ( (fun (x3 : x)
       =>  (kleisli_comp (fun (x4 : x)  =>  (ret x4)) z x3))
    x2) = ( z x2).
  Proof. crush. hammer. Qed.
Lemma lemma_5 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (x) -> (list y)) (x2 : x),
  ( (fun (x3 : x)
       =>  (kleisli_comp z (fun (x4 : y)  =>  (ret x4)) x3))
    x2) = ( z x2).
  Proof. crush. Admitted.
Lemma lemma_6 : 
  forall (x : Type),  forall (y : Type),  forall (z : Type), 
  forall (x2 : (list z)) (x3 : (z) -> (list y))
         (x4 : (y) -> (list x)),
  (bind x2
    (fun (x5 : z)  =>  (kleisli_comp x3 x4 x5))) = (bind (bind x2 x3)
                                                    x4).
  Proof. crush. Admitted.
Lemma lemma_7 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : y) (x2 : (list y)) (x3 : (y) -> (list x)),
  (kleisli_comp (fun (x4 : (list y))  =>  (cons z x4))
    x3 x2) = (bind (cons z x2) x3).
  Proof. crush. Qed.
Lemma lemma_8 : 
  forall (x : Type),  forall (y : Type),  forall (z : Type),  forall (x2 : Type), 
  forall (x3 : (x) -> (list z)) (x4 : (z) -> (list x2))
         (x5 : (x2) -> (list y)) (x6 : x),
  ( (fun (x7 : x)
       => 
      (kleisli_comp (fun (x8 : x)  =>  (kleisli_comp x3 x4 x8)) x5 x7))
    x6) = ( (fun (x7 : x)
               => 
              (kleisli_comp x3 (fun (x9 : z)  =>  (kleisli_comp x4 x5 x9)) x7))
            x6).
  Proof. crush. Admitted.
Lemma lemma_9 : 
  forall (x : Type),  forall (y : Type),  forall (z : Type), 
  forall (x2 : (y) -> (list z)) (x3 : (z) -> (list x)) (x4 : y),
  (bind (bind (ret x4) x2) x3) = (kleisli_comp x2 x3 x4).
  Proof. crush. hammer. Qed.
Lemma lemma_10 : 
  forall (x : Type),  forall (y : Type),  forall (z : Type),  forall (x2 : Type), 
  forall (x3 : ((list z)) -> (list x2)) (x4 : (x2) -> (list y))
         (x5 : (y) -> (list x)),
  (kleisli_comp x3
    (fun (x6 : x2)  =>  (kleisli_comp x4 x5 x6))
    (@nil z)) = (bind (kleisli_comp x3 x4 (@nil z)) x5).
  Proof. crush. Admitted.
Lemma lemma_11 : 
  forall (x : Type),  forall (y : Type),  forall (z : Type),  forall (x2 : Type), 
  forall (x3 : ((list z)) -> (list x2)) (x4 : (x2) -> (list y))
         (x5 : z) (x6 : (y) -> (list x)),
  (kleisli_comp x3
    (fun (x7 : x2)  =>  (kleisli_comp x4 x6 x7))
    (ret x5)) = (bind (kleisli_comp x3 x4 (ret x5)) x6).
  Proof. crush. Admitted.

 *)