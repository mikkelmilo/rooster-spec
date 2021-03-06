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

(* DiscoverLemmas "ZArithTest" ZZ.double ZZ.succ myadd ZZ.pred ZZ.mul ZZ.sub ZZ.opp ZZ.compare ZZ.leb.
Print ZZ.
Import ZZ.
 *)
(*  Lemma lemma_5 : 
  forall (x : positive),
  (compare x x) = eq.
  Proof. Admitted.
Lemma lemma_6 : 
  forall (x : positive),
  (leb x x).
  Proof. Admitted.
Lemma lemma_7 : 
  forall (x : positive) (y : positive),
  (mul y x) = (mul x y).
  Proof. Admitted.
Lemma lemma_8 : 
  forall (x : Z) (y : Z),
  (myadd y x) = (myadd x y).
  Proof. Admitted.
Lemma lemma_9 : 
  forall (x : positive),
  (sub x x) = xH.
  Proof. Admitted.
Lemma lemma_10 : 
  forall (x : positive) (y : positive),
  (-> (leb x y) (sub x y) = xH).
  Proof. Admitted.
Lemma lemma_11 : 
  forall (x : uint),
  (double (d0 x)) = (d0 (double x)).
  Proof. Admitted.
Lemma lemma_12 : 
  forall (x : uint),
  (double (d1 x)) = (d2 (double x)).
  Proof. Admitted.
Lemma lemma_13 : 
  forall (x : uint),
  (double (d2 x)) = (d4 (double x)).
  Proof. Admitted.
Lemma lemma_14 : 
  forall (x : uint),
  (double (d3 x)) = (d6 (double x)).
  Proof. Admitted.
Lemma lemma_15 : 
  forall (x : uint),
  (double (d4 x)) = (d8 (double x)).
  Proof. Admitted.
Lemma lemma_16 : 
  forall (x : positive),
  (leb xH x).
  Proof. Admitted.
Lemma lemma_17 : 
  forall (x : positive),
  (mul x xH) = x.
  Proof. Admitted.
Lemma lemma_18 : 
  forall (x : Z),
  (myadd x z0) = x.
  Proof. Admitted.
Lemma lemma_19 : 
  forall (x : positive),
  (opp (zneg x)) = (zpos x).
  Proof. Admitted.
Lemma lemma_20 : 
  forall (x : positive),
  (opp (zpos x)) = (zneg x).
  Proof. Admitted.
Lemma lemma_21 : 
  forall (x : Z),
  (opp (opp x)) = x.
  Proof. Admitted.
Lemma lemma_22 : 
  forall (x : positive),
  (pred (zneg x)) = (zneg (succ x)).
  Proof. Admitted.
Lemma lemma_23 : 
  forall (x : positive),
  (succ (xI x)) = (xO (succ x)).
  Proof. Admitted.
Lemma lemma_24 : 
  forall (x : positive),
  (succ (xO x)) = (xI x).
  Proof. Admitted.
Lemma lemma_25 : 
  (double (d5 nil)) = (d0 (d1 nil)).
  Proof. Admitted.
Lemma lemma_26 : 
  (double (d6 nil)) = (d2 (d1 nil)).
  Proof. Admitted.
Lemma lemma_27 : 
  (double (d7 nil)) = (d4 (d1 nil)).
  Proof. Admitted.
Lemma lemma_28 : 
  (double (d8 nil)) = (d6 (d1 nil)).
  Proof. Admitted.
Lemma lemma_29 : 
  (double (d9 nil)) = (d8 (d1 nil)).
  Proof. Admitted.
Lemma lemma_30 : 
  (pred (zpos xH)) = z0.
  Proof. Admitted.
Lemma lemma_31 : 
  forall (x : positive),
  (compare x (xI x)) = lt.
  Proof. Admitted.
Lemma lemma_32 : 
  forall (x : positive),
  (compare x (xO x)) = lt.
  Proof. Admitted.
Lemma lemma_33 : 
  forall (x : positive),
  (compare x (succ x)) = lt.
  Proof. Admitted.
Lemma lemma_34 : 
  forall (x : positive),
  (compare (xI x) x) = gt.
  Proof. Admitted.
Lemma lemma_35 : 
  forall (x : positive),
  (compare (xO x) x) = gt.
  Proof. Admitted.
Lemma lemma_36 : 
  forall (x : positive),
  (compare (succ x) x) = gt.
  Proof. Admitted.
Lemma lemma_37 : 
  forall (x : positive),
  (leb x (xI x)).
  Proof. Admitted.
Lemma lemma_38 : 
  forall (x : positive),
  (leb x (xO x)).
  Proof. Admitted.
Lemma lemma_39 : 
  forall (x : positive),
  (leb x (succ x)).
  Proof. Admitted.
Lemma lemma_40 : 
  forall (x : positive),
  (not (leb (xI x) x)).
  Proof. Admitted.
Lemma lemma_41 : 
  forall (x : positive),
  (not (leb (xO x) x)).
  Proof. Admitted.
Lemma lemma_42 : 
  forall (x : positive),
  (not (leb (succ x) x)).
  Proof. Admitted.
Lemma lemma_43 : 
  forall (x : positive) (y : positive),
  (mul x (xO y)) = (xO (mul x y)).
  Proof. Admitted.
Lemma lemma_44 : 
  forall (x : Z),
  (myadd x (opp x)) = z0.
  Proof. Admitted.
Lemma lemma_45 : 
  forall (x : Z) (y : Z),
  (myadd y (pred x)) = (myadd x (pred y)).
  Proof. Admitted.
Lemma lemma_46 : 
  forall (x : Z) (y : Z),
  (pred (myadd x y)) = (myadd x (pred y)).
  Proof. Admitted.
Lemma lemma_47 : 
  forall (x : positive),
  (succ x) = (sub (xI x) x).
  Proof. Admitted.
Lemma lemma_48 : 
  forall (x : positive),
  (sub (xO x) x) = x.
  Proof. Admitted.
Lemma lemma_49 : 
  forall (x : positive),
  (sub (succ x) x) = xH.
  Proof. Admitted.
Lemma lemma_50 : 
  forall (x : positive),
  (compare xH (xI x)) = lt.
  Proof. Admitted.
Lemma lemma_51 : 
  forall (x : positive),
  (compare xH (xO x)) = lt.
  Proof. Admitted.
Lemma lemma_52 : 
  forall (x : positive),
  (compare xH (succ x)) = lt.
  Proof. Admitted.
Lemma lemma_53 : 
  forall (x : positive),
  (compare (xI x) xH) = gt.
  Proof. Admitted.
Lemma lemma_54 : 
  forall (x : positive),
  (compare (xO x) xH) = gt.
  Proof. Admitted.
Lemma lemma_55 : 
  forall (x : positive),
  (compare (succ x) xH) = gt.
  Proof. Admitted.
Lemma lemma_56 : 
  forall (x : uint),
  (double (d5 (d0 x))) = (d0 (d1 (double x))).
  Proof. Admitted.
Lemma lemma_57 : 
  forall (x : uint),
  (double (d5 (d1 x))) = (d0 (d3 (double x))).
  Proof. Admitted.
Lemma lemma_58 : 
  forall (x : uint),
  (double (d5 (d2 x))) = (d0 (d5 (double x))).
  Proof. Admitted.
Lemma lemma_59 : 
  forall (x : uint),
  (double (d5 (d3 x))) = (d0 (d7 (double x))).
  Proof. Admitted.
Lemma lemma_60 : 
  forall (x : uint),
  (double (d5 (d4 x))) = (d0 (d9 (double x))).
  Proof. Admitted.
Lemma lemma_61 : 
  forall (x : uint),
  (double (d6 (d0 x))) = (d2 (d1 (double x))).
  Proof. Admitted.
Lemma lemma_62 : 
  forall (x : uint),
  (double (d6 (d1 x))) = (d2 (d3 (double x))).
  Proof. Admitted.
Lemma lemma_63 : 
  forall (x : uint),
  (double (d6 (d2 x))) = (d2 (d5 (double x))).
  Proof. Admitted.
Lemma lemma_64 : 
  forall (x : uint),
  (double (d6 (d3 x))) = (d2 (d7 (double x))).
  Proof. Admitted.
Lemma lemma_65 : 
  forall (x : uint),
  (double (d6 (d4 x))) = (d2 (d9 (double x))).
  Proof. Admitted.
Lemma lemma_66 : 
  forall (x : uint),
  (double (d7 (d0 x))) = (d4 (d1 (double x))).
  Proof. Admitted.
Lemma lemma_67 : 
  forall (x : uint),
  (double (d7 (d1 x))) = (d4 (d3 (double x))).
  Proof. Admitted.
Lemma lemma_68 : 
  forall (x : uint),
  (double (d7 (d2 x))) = (d4 (d5 (double x))).
  Proof. Admitted.
Lemma lemma_69 : 
  forall (x : uint),
  (double (d7 (d3 x))) = (d4 (d7 (double x))).
  Proof. Admitted.
Lemma lemma_70 : 
  forall (x : uint),
  (double (d7 (d4 x))) = (d4 (d9 (double x))).
  Proof. Admitted.
Lemma lemma_71 : 
  forall (x : uint),
  (double (d8 (d0 x))) = (d6 (d1 (double x))).
  Proof. Admitted.
Lemma lemma_72 : 
  forall (x : uint),
  (double (d8 (d1 x))) = (d6 (d3 (double x))).
  Proof. Admitted.
Lemma lemma_73 : 
  forall (x : uint),
  (double (d8 (d2 x))) = (d6 (d5 (double x))).
  Proof. Admitted.
Lemma lemma_74 : 
  forall (x : uint),
  (double (d8 (d3 x))) = (d6 (d7 (double x))).
  Proof. Admitted.
Lemma lemma_75 : 
  forall (x : uint),
  (double (d8 (d4 x))) = (d6 (d9 (double x))).
  Proof. Admitted.
Lemma lemma_76 : 
  forall (x : uint),
  (double (d9 (d0 x))) = (d8 (d1 (double x))).
  Proof. Admitted.
Lemma lemma_77 : 
  forall (x : uint),
  (double (d9 (d1 x))) = (d8 (d3 (double x))).
  Proof. Admitted.
Lemma lemma_78 : 
  forall (x : uint),
  (double (d9 (d2 x))) = (d8 (d5 (double x))).
  Proof. Admitted.
Lemma lemma_79 : 
  forall (x : uint),
  (double (d9 (d3 x))) = (d8 (d7 (double x))).
  Proof. Admitted.
Lemma lemma_80 : 
  forall (x : uint),
  (double (d9 (d4 x))) = (d8 (d9 (double x))).
  Proof. Admitted.
Lemma lemma_81 : 
  forall (x : positive),
  (not (leb (xI x) xH)).
  Proof. Admitted.
Lemma lemma_82 : 
  forall (x : positive),
  (not (leb (xO x) xH)).
  Proof. Admitted.
Lemma lemma_83 : 
  forall (x : positive),
  (not (leb (succ x) xH)).
  Proof. Admitted.
Lemma lemma_84 : 
  forall (x : Z),
  (myadd x (zpos xH)) = (opp (pred (opp x))).
  Proof. Admitted.
Lemma lemma_85 : 
  forall (x : positive),
  (pred (zpos (xI x))) = (zpos (xO x)).
  Proof. Admitted.
Lemma lemma_86 : 
  forall (x : positive),
  (pred (zpos (succ x))) = (zpos x).
  Proof. Admitted.
Lemma lemma_87 : 
  forall (x : Z),
  (pred (opp (pred x))) = (opp x).
  Proof. Admitted.
Lemma lemma_88 : 
  forall (x : positive),
  (sub (xI x) xH) = (xO x).
  Proof. Admitted.
Lemma lemma_89 : 
  forall (x : positive),
  (sub (xO xH) x) = xH.
  Proof. Admitted.
Lemma lemma_90 : 
  forall (x : positive),
  (sub (succ x) xH) = x.
  Proof. Admitted.
Lemma lemma_91 : 
  (double (d5 (d5 nil))) = (d0 (d1 (d1 nil))).
  Proof. Admitted.
Lemma lemma_92 : 
  (double (d5 (d6 nil))) = (d0 (d3 (d1 nil))).
  Proof. Admitted.
Lemma lemma_93 : 
  (double (d5 (d7 nil))) = (d0 (d5 (d1 nil))).
  Proof. Admitted.
Lemma lemma_94 : 
  (double (d5 (d8 nil))) = (d0 (d7 (d1 nil))).
  Proof. Admitted.
Lemma lemma_95 : 
  (double (d5 (d9 nil))) = (d0 (d9 (d1 nil))).
  Proof. Admitted.
Lemma lemma_96 : 
  (double (d6 (d5 nil))) = (d2 (d1 (d1 nil))).
  Proof. Admitted.
Lemma lemma_97 : 
  (double (d6 (d6 nil))) = (d2 (d3 (d1 nil))).
  Proof. Admitted.
Lemma lemma_98 : 
  (double (d6 (d7 nil))) = (d2 (d5 (d1 nil))).
  Proof. Admitted.
Lemma lemma_99 : 
  (double (d6 (d8 nil))) = (d2 (d7 (d1 nil))).
  Proof. Admitted.
Lemma lemma_100 : 
  (double (d6 (d9 nil))) = (d2 (d9 (d1 nil))).
  Proof. Admitted.
Lemma lemma_101 : 
  (double (d7 (d5 nil))) = (d4 (d1 (d1 nil))).
  Proof. Admitted.
Lemma lemma_102 : 
  (double (d7 (d6 nil))) = (d4 (d3 (d1 nil))).
  Proof. Admitted.
Lemma lemma_103 : 
  (double (d7 (d7 nil))) = (d4 (d5 (d1 nil))).
  Proof. Admitted.
Lemma lemma_104 : 
  (double (d7 (d8 nil))) = (d4 (d7 (d1 nil))).
  Proof. Admitted.
Lemma lemma_105 : 
  (double (d7 (d9 nil))) = (d4 (d9 (d1 nil))).
  Proof. Admitted.
Lemma lemma_106 : 
  (double (d8 (d5 nil))) = (d6 (d1 (d1 nil))).
  Proof. Admitted.
Lemma lemma_107 : 
  (double (d8 (d6 nil))) = (d6 (d3 (d1 nil))).
  Proof. Admitted.
Lemma lemma_108 : 
  (double (d8 (d7 nil))) = (d6 (d5 (d1 nil))).
  Proof. Admitted.
Lemma lemma_109 : 
  (double (d8 (d8 nil))) = (d6 (d7 (d1 nil))).
  Proof. Admitted.
Lemma lemma_110 : 
  (double (d8 (d9 nil))) = (d6 (d9 (d1 nil))).
  Proof. Admitted.
Lemma lemma_111 : 
  (double (d9 (d5 nil))) = (d8 (d1 (d1 nil))).
  Proof. Admitted.
Lemma lemma_112 : 
  (double (d9 (d6 nil))) = (d8 (d3 (d1 nil))).
  Proof. Admitted.
Lemma lemma_113 : 
  (double (d9 (d7 nil))) = (d8 (d5 (d1 nil))).
  Proof. Admitted.
Lemma lemma_114 : 
  (double (d9 (d8 nil))) = (d8 (d7 (d1 nil))).
  Proof. Admitted.
Lemma lemma_115 : 
  (double (d9 (d9 nil))) = (d8 (d9 (d1 nil))).
  Proof. Admitted.
 *)
