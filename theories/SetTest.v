From Coq Require Import Extraction.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.

From Coq Require Import Lists.ListSet.
From Coq Require Import List. 
Import ListNotations.

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
MaxTestSize 15.

(* Lemma lemma_2 : 
  forall (x : Type), 
  forall (y : x0 y : x, {x0 = y} + {x0 <> y}) (z : x),
  (set_add y z (@nil x)) = (cons z (@nil x)).
  hammer. Qed.



DiscoverLemmas "SetTest" set_add empty_set set_mem set_remove set_inter set_union set_diff set_prod set_map map. *)