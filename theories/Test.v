From Coq Require Import Extraction.
From Coq Require Import List.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.
Add LoadPath "~/Documents/cpdt/src" as Cpdt.
Require Import Cpdt.CpdtTactics.



(* DiscoverLemmas "Test" length rev rev_append hd tail map fold_right existsb forallb. *)

(* 
Lemma lemma_1 : 
  forall (x : Type), 
  (length (@nil x)) = O.
  hammer. Qed.
Lemma lemma_2 : 
  forall (x : Type), 
  (rev (@nil x)) = (@nil x).
  hammer. Qed.
Lemma lemma_3 : 
  forall (x : Type), 
  (tl (@nil x)) = (@nil x).
  hammer. Qed.
Lemma lemma_4 : 
  forall (x : bool) (y : bool),
  (andb y x) = (andb x y).
  hammer. Qed.
Lemma lemma_5 : 
  forall (x : bool),
  (andb x x) = x.
  hammer. Qed.
Lemma lemma_6 : 
  forall (x : bool) (y : bool),
  (orb y x) = (orb x y).
  hammer. Qed.
Lemma lemma_7 : 
  forall (x : bool),
  (orb x x) = x.
  hammer. Qed.
Lemma lemma_8 : 
  forall (x : bool),
  (andb x false) = false.
  hammer. Qed.
Lemma lemma_9 : 
  forall (x : bool),
  (andb x true) = x.
  hammer. Qed.
Lemma lemma_10 : 
  forall (x : Type), 
  forall (y : (list x)),
  (app y (@nil x)) = y.
  hammer. Qed.
Lemma lemma_11 : 
  forall (x : Type), 
  forall (y : (list x)),
  (app (@nil x) y) = y.
  hammer. Qed.
Lemma lemma_12 : 
  forall (x : Type), 
  forall (y : (x) -> bool),
  (existsb y (@nil x)) = false.
  hammer. Qed.
Lemma lemma_13 : 
  forall (x : Type), 
  forall (y : (x) -> bool),
  (forallb y (@nil x)) = true.
  hammer. Qed.
Lemma lemma_14 : 
  forall (x : Type), 
  forall (y : x),
  (hd y (@nil x)) = y.
  hammer. Qed.
Lemma lemma_15 : 
  forall (x : Type), 
  forall (y : (list x)),
  (length (rev y)) = (length y).
  hammer. Qed.
Lemma lemma_16 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (x) -> y),
  (map z (@nil x)) = (@nil y).
  hammer. Qed.
Lemma lemma_17 : 
  forall (x : bool),
  (orb x false) = x.
  hammer. Qed.
Lemma lemma_18 : 
  forall (x : bool),
  (orb x true) = true.
  hammer. Qed.
Lemma lemma_19 : 
  forall (x : Type), 
  forall (y : (list x)),
  (rev (rev y)) = y.
  hammer. Qed.
Lemma lemma_20 : 
  forall (x : Type), 
  forall (y : (list x)),
  (rev_append y (@nil x)) = (rev y).
  hammer. Qed.
Lemma lemma_21 : 
  forall (x : Type), 
  forall (y : (list x)),
  (rev_append (@nil x) y) = y.
  hammer. Qed.
Lemma lemma_22 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (app (rev y) z) = (rev_append y z).
  hammer. Qed.
Lemma lemma_23 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)),
  (existsb y (rev z)) = (existsb y z).
  hammer. Qed.
Lemma lemma_24 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (x) -> (y) -> y) (x2 : y),
  (fold_right z x2 (@nil x)) = x2.
  hammer. Qed.
Lemma lemma_25 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)),
  (forallb y (rev z)) = (forallb y z).
  hammer. Qed.
Lemma lemma_26 : 
  forall (x : Type), 
  forall (y : x) (z : (list x)),
  (length (cons y z)) = (S (length z)).
  hammer. Qed.
Lemma lemma_27 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (length (app z y)) = (length (app y z)).
  hammer. Qed.
Lemma lemma_28 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (y) -> x) (x2 : (list y)),
  (length (map z x2)) = (length x2).
  hammer. Qed.
Lemma lemma_29 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (length (rev_append y z)) = (length (app y z)).
  hammer. Qed.
Lemma lemma_30 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (y) -> x) (x2 : (list y)),
  (rev (map z x2)) = (map z (rev x2)).
  hammer. Qed.
Lemma lemma_31 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (rev (rev_append z y)) = (rev_append y z).
  hammer. Qed.
Lemma lemma_32 : 
  forall (x : Type), 
  forall (y : (list x)),
  (rev_append (tl y) y) = (rev_append y (tl y)).
  hammer. Qed.
Lemma lemma_33 : 
  forall (x : Type), 
  forall (y : x) (z : (list x)),
  (tl (cons y z)) = z.
  hammer. Qed.
Lemma lemma_34 : 
  forall (x : Type), 
  forall (y : (list x)),
  (tl (app y y)) = (app (tl y) y).
  hammer. Qed.
Lemma lemma_35 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (y) -> x) (x2 : (list y)),
  (tl (map z x2)) = (map z (tl x2)).
  hammer. Qed.
Lemma lemma_36 : 
  forall (x : Type), 
  forall (y : (list x)),
  (length (tl (rev y))) = (length (tl y)).
  hammer. Qed.
Lemma lemma_37 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (fold_right (fun (x2 : x)
                 =>  (fun (x3 : (list x))  =>  (cons x2 x3)))
    z y) = (app y z).
  hammer. Qed.
Lemma lemma_38 : 
  forall (x : bool) (y : bool) (z : bool),
  (andb (andb x y) z) = (andb x (andb y z)).
  hammer. Qed.
Lemma lemma_39 : 
  forall (x : bool) (y : bool),
  (andb y (orb y x)) = y.
  hammer. Qed.
Lemma lemma_40 : 
  forall (x : Type), 
  forall (y : x) (z : (list x)) (x2 : (list x)),
  (app (cons y z) x2) = (cons y (app z x2)).
  hammer. Qed.
Lemma lemma_41 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)) (x2 : (list x)),
  (app (app y z) x2) = (app y (app z x2)).
  hammer. Qed.
Lemma lemma_42 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)) (x2 : (list x)),
  (existsb y (app x2 z)) = (existsb y (app z x2)).
  hammer. Qed.
Lemma lemma_43 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)),
  (existsb y (app z z)) = (existsb y z).
  hammer. Qed.
Lemma lemma_44 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)) (x2 : (list x)),
  (existsb y (rev_append z x2)) = (existsb y (app z x2)).
  hammer. Qed.
Lemma lemma_45 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)) (x2 : (list x)),
  (forallb y (app x2 z)) = (forallb y (app z x2)).
  hammer. Qed.
Lemma lemma_46 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)),
  (forallb y (app z z)) = (forallb y z).
  hammer. Qed.
Lemma lemma_47 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : (list x)) (x2 : (list x)),
  (forallb y (rev_append z x2)) = (forallb y (app z x2)).
  hammer. Qed.
Lemma lemma_48 : 
  forall (x : Type), 
  forall (y : (list x)) (z : x) (x2 : x) (x3 : (list x)),
  (hd z (cons x2 y)) = (hd z (cons x2 x3)).
  hammer. Qed.
Lemma lemma_49 : 
  forall (x : Type), 
  forall (y : x) (z : (list x)) (x2 : x),
  (hd y (cons x2 z)) = x2.
  hammer. Qed.
Lemma lemma_50 : 
  forall (x : Type), 
  forall (y : x) (z : (list x)),
  (hd y (app z z)) = (hd y z).
  hammer. Qed.
Lemma lemma_51 : 
  forall (x : Type), 
  forall (y : x) (z : (list x)),
  (hd y (rev_append z z)) = (hd y (rev z)).
  hammer. Qed.
Lemma lemma_52 : 
  forall (x : Type), 
  forall (y : x) (z : (list x)) (x2 : (list x)),
  (hd (hd y x2) z) = (hd y (app z x2)).
  hammer. Qed.
Lemma lemma_53 : 
  forall (x : bool) (y : bool),
  (orb y (andb y x)) = y.
  hammer. Qed.
Lemma lemma_54 : 
  forall (x : bool) (y : bool) (z : bool),
  (orb (orb x y) z) = (orb x (orb y z)).
  hammer. Qed.
Lemma lemma_55 : 
  forall (x : Type), 
  forall (y : (list x)) (z : x) (x2 : (list x)),
  (rev_append (cons z y) x2) = (rev_append y (cons z x2)).
  hammer. Qed.
Lemma lemma_56 : 
  forall (x : Type), 
  forall (y : (list x)),
  (app (tl (rev y)) y) = (tl (rev_append y y)).
  hammer. Qed.
Lemma lemma_57 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : x),
  (existsb y (cons z (@nil x))) = (@ y z).
  hammer. Qed.
Lemma lemma_58 : 
  forall (x : Type), 
  forall (y : (x) -> bool) (z : x),
  (forallb y (cons z (@nil x))) = (@ y z).
  hammer. Qed.
Lemma lemma_59 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (length (tl (app z y))) = (length (tl (app y z))).
  hammer. Qed.
Lemma lemma_60 : 
  forall (x : Type), 
  forall (y : (list x)) (z : (list x)),
  (length (tl (rev_append y z))) = (length (tl (app y z))).
  hammer. Qed.
Lemma lemma_61 : 
  forall (x : Type),  forall (y : Type), 
  forall (z : (y) -> x) (x2 : y),
  (map z (cons x2 (@nil y))) = (cons (@ z x2) (@nil x)).
  hammer. Qed.
Lemma lemma_62 : 
  forall (x : Type), 
  forall (y : (list x)),
  (tl (app y (tl y))) = (app (tl y) (tl y)).
  hammer. Qed.
Lemma lemma_63 : 
  forall (x : Type), 
  forall (y : (list x)),
  (length (tl (tl (rev y)))) = (length (tl (tl y))).
  hammer. Qed.
Lemma lemma_64 : 
  forall (x : Type), 
  forall (y : (list x)),
  (tl (rev (tl (rev y)))) = (rev (tl (rev (tl y)))).
  hammer. Qed.

 *)