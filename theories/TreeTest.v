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
Extract Inlined Constant negb => "Prelude.not".

Require Import NPeano.

(* Definition V := nat.
Definition default := 0.
Definition key := nat.
Inductive tree : Type :=
 | E : tree
 | T: tree -> key -> V -> tree -> tree.
Definition empty_tree : tree := E.

Fixpoint nat_lt_bool (n1 n2 :nat) : bool :=
  match (n1, n2) with
  | (0, 0) => false
  | (0,S _) => true
  | (S _, 0) => false
  | (S n1', S n2') => nat_lt_bool n1' n2'
  end.

Fixpoint lookup (x: key) (t : tree) : V :=
  match t with
  | E => default
  | T tl k v tr => if nat_lt_bool x k then lookup x tl
                         else if nat_lt_bool k x then lookup x tr
                         else v
  end.
Fixpoint insert (x: key) (v: V) (s: tree) : tree :=
 match s with
 | E => T E x v E
 | T a y v' b => if nat_lt_bool x y then T (insert x v a) y v' b
                        else if nat_lt_bool y x then T a y v' (insert x v b)
                        else T a x v b
 end.
Fixpoint elements' (s: tree) (base: list (key*V)) : list (key * V) :=
 match s with
 | E => base
 | T a k v b => elements' a ((k,v) :: elements' b base)
 end.
Definition elements (s: tree) : list (key * V) := elements' s nil.
 *)


Inductive tree A :=
| leaf : tree A
| node : A -> tree A -> tree A -> tree A.

Arguments leaf {A}.
Arguments node {A} _ _ _.

Fixpoint mirror {A : Type} (t : tree A) : tree A :=
  match t with
    | leaf => leaf
    | node x l r => node x (mirror r) (mirror l)
  end.


Definition rotate_right {A : Type} (t : tree A) : tree A :=
  match t with
  | leaf => leaf
  | node x leaf r => t
  | node x (node y il ir) r => node y il (node x ir r)
  end.
  
Definition rotate_left {A : Type} (t : tree A) : tree A :=
  match t with
  | leaf => leaf
  | node x l leaf => t
  | node x l (node y il ir) => node y (node x l il) ir
  end.
  
Fixpoint map_tree {A B : Type} (f : A -> B) (t : tree A) : tree B :=
  match t with
  | leaf => leaf
  | node x l r => node (f x) (map_tree f l) (map_tree f r)
  end. 
  
(* Definition is_nonempty {A : Type} (t : tree A) : bool :=
  match t with
  | leaf => false
  | _ => true
  end. *)
  
MaxTermSize 8.
MaxTestSize 20.

DiscoverLemmas "treesTest" mirror rotate_left rotate_right map_tree.  

(* Lemma lemma_1 : 
  forall (x : Type), 
  (mirror (@leaf x)) = (@leaf x).
  Proof. hammer.  Qed.
Lemma lemma_2 : 
  forall (x : Type), 
  (rotate_left (@leaf x)) = (@leaf x).
  Proof. hammer.  Qed.
Lemma lemma_3 : 
  forall (x : Type), 
  (rotate_right (@leaf x)) = (@leaf x).
  Proof. hammer.  Qed.
Lemma lemma_4 : 
  forall (x : Type), 
  forall (y : (tree x)),
  (mirror (mirror y)) = y.
  Proof. crush. Admitted.
Lemma lemma_5 : 
  forall (x : Type), 
  forall (y : (tree x)),
  (rotate_left (mirror y)) = (mirror (rotate_right y)).
  Proof. hammer. Qed.
Lemma lemma_6 : 
  forall (x : Type), 
  forall (y : (tree x)),
  (rotate_left (rotate_left (rotate_right y))) = (rotate_left (rotate_left y)).
  Proof. crush. Admitted.
Lemma lemma_7 : 
  forall (x : Type), 
  forall (y : (tree x)),
  (rotate_left (rotate_right (rotate_left y))) = (rotate_left y).
  Proof. hammer. Qed.
Lemma lemma_8 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (mirror (node y z (@leaf x))) = (node y (@leaf x) (mirror z)).
  Proof. hammer. Qed.
Lemma lemma_9 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (node y z (@leaf x))) = (node y z (@leaf x)).
  Proof.  hammer. Qed.
Lemma lemma_10 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)) (x2 : (tree x)),
  (node y (mirror x2) (mirror z)) = (mirror (node y z x2)).
  Proof.  hammer. Qed.
Lemma lemma_11 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y z z))) = (node y z z).
  Proof.  hammer. Qed.
Lemma lemma_12 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y z (mirror z)))) = (node y
                                                         z (mirror z)).
  Proof.  hammer. Qed.
Lemma lemma_13 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y z (rotate_left z)))) = (node y
                                                              z (rotate_left z)).
  Proof.  hammer. Qed.
Lemma lemma_14 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y z (rotate_right z)))) = (node y
                                                               z (rotate_right z)).
  Proof.  hammer. Qed.
Lemma lemma_15 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y (rotate_left z) z))) = (node y
                                                              (rotate_left z) z).
  Proof.  hammer. Qed.
Lemma lemma_16 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y (rotate_right z) z))) = (node y
                                                               (rotate_right z) z).
  Proof.  hammer. Qed.
Lemma lemma_17 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)) (x2 : x) (x3 : (tree x))
         (x4 : (tree x)),
  (node x2 (node y z x3) x4) = (rotate_left (node y
                                              z (node x2 x3 x4))).
  Proof.  hammer. Qed.
Lemma lemma_18 : 
  forall (x : Type), 
  forall (y : x) (z : x) (x2 : (tree x)) (x3 : (tree x))
         (x4 : (tree x)),
  (node z x2 (node y x3 x4)) = (rotate_right (node y
                                               (node z x2 x3) x4)).
  Proof.  hammer. Qed.
Lemma lemma_19 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               z (mirror (rotate_left z))))) = (node y
                                                                 z (mirror (rotate_left z))).
  Proof.  hammer. Qed.
Lemma lemma_20 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               z (mirror (rotate_right z))))) = (node y
                                                                  z (mirror (rotate_right z))).
  Proof.  hammer. Qed.
Lemma lemma_21 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               z (rotate_left (rotate_left z))))) = (node y
                                                                      z
                                                                      (rotate_left (rotate_left z))).
  Proof.  hammer. Qed.
Lemma lemma_22 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               z (rotate_left (rotate_right z))))) = (node y
                                                                       z
                                                                       (rotate_left (rotate_right z))).
  Proof.  hammer. Qed.
Lemma lemma_23 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               z (rotate_right (rotate_left z))))) = (node y
                                                                       z
                                                                       (rotate_right (rotate_left z))).
  Proof.  hammer. Qed.
Lemma lemma_24 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               z (rotate_right (rotate_right z))))) = (node y
                                                                        z
                                                                        (rotate_right (rotate_right z))).
  Proof.  hammer. Qed.
Lemma lemma_25 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (mirror (rotate_left z)) z))) = (node y
                                                                 (mirror (rotate_left z)) z).
  Proof.  hammer. Qed.
Lemma lemma_26 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (mirror (rotate_right z)) z))) = (node y
                                                                  (mirror (rotate_right z)) z).
  Proof.  hammer. Qed.
Lemma lemma_27 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (rotate_left z) (rotate_right z)))) = (node y
                                                                       (rotate_left z)
                                                                       (rotate_right z)).
  Proof.  hammer. Qed.
Lemma lemma_28 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (rotate_left (rotate_left z)) z))) = (node y
                                                                      (rotate_left (rotate_left z))
                                                                      z).
  Proof.  hammer. Qed.
Lemma lemma_29 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (rotate_left (rotate_right z)) z))) = (node y
                                                                       (rotate_left (rotate_right z))
                                                                       z).
  Proof. crush. hammer. Qed.
Lemma lemma_30 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (rotate_right z) (rotate_left z)))) = (node y
                                                                       (rotate_right z)
                                                                       (rotate_left z)).
  Proof.  hammer. Qed.
Lemma lemma_31 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (rotate_right (rotate_left z)) z))) = (node y
                                                                       (rotate_right (rotate_left z))
                                                                       z).
  Proof. hammer. Qed.
Lemma lemma_32 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_left (rotate_right (node y
                               (rotate_right (rotate_right z)) z))) = (node y
                                                                        (rotate_right (rotate_right z))
                                                                        z).
  Proof. hammer. Qed.
Lemma lemma_33 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_right (rotate_left (node y
                               (rotate_left z) (rotate_right z)))) = (node y
                                                                       (rotate_left z)
                                                                       (rotate_right z)).
  Proof. hammer. Qed.
Lemma lemma_34 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)),
  (rotate_right (rotate_left (node y
                               (rotate_right z) (rotate_left z)))) = (node y
                                                                       (rotate_right z)
                                                                       (rotate_left z)).
  Proof. hammer. Qed.
Lemma lemma_35 : 
  forall (x : Type), 
  forall (y : x) (z : (tree x)) (x2 : (tree x)),
  (rotate_right (rotate_right (node y
                                (rotate_right (rotate_right z)) x2))) = (rotate_right (node y
                                                                                        (rotate_right (rotate_right z))
                                                                                        x2)).
  Proof. hammer. Qed.
 *)

