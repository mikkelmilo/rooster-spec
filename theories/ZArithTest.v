From Coq Require Import Extraction.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.

From Coq Require Import ZArith.BinIntDef.
Module ZZ := BinIntDef.Z.
Print ZZ.

MaxTermSize 4.
MaxTestSize 12.


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


Definition myadd := ZZ.add.

DiscoverLemmas "ZArithTest" ZZ.double ZZ.succ myadd ZZ.pred ZZ.mul ZZ.sub ZZ.opp ZZ.compare ZZ.leb.
Print ZZ.
