From Coq Require Import Extraction.
From Coq Require Import List.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.
Add LoadPath "~/Documents/cpdt/src" as Cpdt.
Require Import Cpdt.CpdtTactics.
Open Scope list_scope.

MaxTermSize 5.
MaxTestSize 20.

From Coq Require Import ZArith.BinIntDef.
Module ZZ := BinIntDef.Z.
Print ZZ.


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

(* not needed to prove the laws of groups, but can be useful for clarity *)
Definition isUnit (n : Z) := ZZ.eqb n ZZ.zero.

Definition zadd := ZZ.add.

(* DiscoverLemmas "ZGroupTest" zadd isUnit. *)

(* result with zadd:
== Laws ==
  1. zadd x y = zadd y x           
  2. zadd x Z0 = x                   
  3. zadd (zadd x y) z = zadd x (zadd y z)
  4. zadd (Zneg x) (Zneg x) = Zneg (XO x)
  5. zadd (Zneg x) (Zpos x) = Z0     
  6. zadd (Zpos x) (Zpos x) = Zpos (XO x)

  3. is associativity law for group
  2. and 1. implies identity element law for group
  5. and 1. implies inverse element law for group 
  Note: the Closure law doesn't make sense here because it is internalized in the underlying type theory.
 *)
 
(* results with isUnit included:
  1. isUnit x & isUnit y => x = y 
  2. isUnit x => Z0 = x            
  3. zadd x y = zadd y x           
  4. isUnit y => zadd x y = x      
  5. isUnit (Zneg x) = False         
  6. isUnit (Zpos x) = False         
  7. isUnit (zadd x x) = isUnit x    
  8. zadd (zadd x y) z = zadd x (zadd y z)
  9. zadd (Zneg x) (Zneg x) = Zneg (XO x)
 10. isUnit y => zadd (Zneg x) (Zpos x) = y
 11. zadd (Zpos x) (Zpos x) = Zpos (XO x)

  1. is the law of uniqueness of unit
  4. is another way of formulating the identity law.
  10. is another way of formulating the inverse element law.
*)
 
(*  Lemma lemma_1 : 
  forall (x : Z) (y : Z),
  (zadd y x) = (zadd x y).
  Proof. crush. Qed.
Lemma lemma_2 : 
  forall (x : Z),
  (zadd x Z0) = x.
  Proof. crush. Qed.
Lemma lemma_3 : 
  forall (x : Z) (y : Z) (z : Z),
  (zadd (zadd x y) z) = (zadd x (zadd y z)).
  Proof. crush. Qed.
Lemma lemma_4 : 
  forall (x : positive),
  (zadd (Zneg x) (Zneg x)) = (Zneg (xO x)).
  Proof. hammer. Qed.
Lemma lemma_5 : 
  forall (x : positive),
  (zadd (Zneg x) (Zpos x)) = Z0.
  Proof. crush. hammer. Qed.
Lemma lemma_6 : 
  forall (x : positive),
  (zadd (Zpos x) (Zpos x)) = (Zpos (xO x)).
  Proof. crush. hammer. Qed. *)

