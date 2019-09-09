{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module A where
import qualified Text.Show.Functions
import qualified Data.Constraint as QS
import qualified Data.Typeable as T
import qualified GHC.Generics as G
import qualified Prelude as P
import qualified QuickSpec as QS
import qualified Test.Feat as F
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen.Unsafe as QU
data Pair a b = Pair2 a b
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Pair)
instance
  (F.Enumerable a, F.Enumerable b) => QC.Arbitrary (Pair a b)
  where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance
  (QC.Arbitrary a, QC.CoArbitrary a, QC.Arbitrary b,
   QC.CoArbitrary b) =>
    QC.CoArbitrary (Pair a b)
  where
  coarbitrary = QC.genericCoarbitrary
data List c = Nil | Cons c (List c)
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''List)
instance (F.Enumerable c) => QC.Arbitrary (List c) where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance
  (QC.Arbitrary c, QC.CoArbitrary c) => QC.CoArbitrary (List c)
  where
  coarbitrary = QC.genericCoarbitrary
data Uint =
  Nil2 | D0 Uint | D1 Uint | D2 Uint | D3 Uint | D4 Uint | D5 Uint |
  D6 Uint | D7 Uint | D8 Uint | D9 Uint
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Uint)
instance QC.Arbitrary Uint where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Uint where
  coarbitrary = QC.genericCoarbitrary
data Reflect = ReflectT | ReflectF
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Reflect)
instance QC.Arbitrary Reflect where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Reflect where
  coarbitrary = QC.genericCoarbitrary
data Positive = XI Positive | XO Positive | XH
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Positive)
instance QC.Arbitrary Positive where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Positive where
  coarbitrary = QC.genericCoarbitrary
data Z = Z0 | Zpos Positive | Zneg Positive
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Z)
instance QC.Arbitrary Z where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Z where coarbitrary = QC.genericCoarbitrary
data Nat = O | S Nat
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Nat)
instance QC.Arbitrary Nat where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Nat where
  coarbitrary = QC.genericCoarbitrary
data Tree = Leaf | Node Z Tree Nat Tree
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Tree)
instance QC.Arbitrary Tree where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Tree where
  coarbitrary = QC.genericCoarbitrary
data Triple = Mktriple Tree P.Bool Tree
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Triple)
instance QC.Arbitrary Triple where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Triple where
  coarbitrary = QC.genericCoarbitrary
data Maybe a2 = Nothing | Just a2
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Maybe)
instance (F.Enumerable a2) => QC.Arbitrary (Maybe a2) where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance
  (QC.Arbitrary a2, QC.CoArbitrary a2) => QC.CoArbitrary (Maybe a2)
  where
  coarbitrary = QC.genericCoarbitrary
data Int2 = Pos Uint | Neg Uint
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Int2)
instance QC.Arbitrary Int2 where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Int2 where
  coarbitrary = QC.genericCoarbitrary
data Enumeration = End | More Nat Tree Enumeration
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Enumeration)
instance QC.Arbitrary Enumeration where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Enumeration where
  coarbitrary = QC.genericCoarbitrary
data Comparison = Eq2 | Lt | Gt
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''Comparison)
instance QC.Arbitrary Comparison where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary Comparison where
  coarbitrary = QC.genericCoarbitrary
data CompareSpecT = CompEqT | CompLtT | CompGtT
  deriving (P.Eq, P.Ord, P.Show, G.Generic, T.Typeable)
F.deriveEnumerable (''CompareSpecT)
instance QC.Arbitrary CompareSpecT where
  arbitrary =
    do k <- QC.sized P.return
       n <- QC.choose (0, (k P.* (2)) P.+ (2))
       F.uniform n
instance QC.CoArbitrary CompareSpecT where
  coarbitrary = QC.genericCoarbitrary
newtype () =
  Mk
    (forall any .
       (QC.Arbitrary any, F.Enumerable any, P.Ord any) => any)
get (Mk x) = x
(.) ::
  forall any .
    (QC.Arbitrary any, F.Enumerable any, P.Ord any, ?imp :: ()) => any
(.) = get (?imp)
instance QC.Arbitrary () where
  arbitrary =
    do x <- QU.capture
       case x of QU.Capture y -> P.return (Mk (y QC.arbitrary))
gen :: QC.Gen (QS.Dict (?imp :: ()))
gen =
  do x <- QC.arbitrary
     let ?imp = x in P.return QS.Dict
xorb :: forall . (?imp :: ()) => P.Bool -> P.Bool -> P.Bool
xorb P.True z = P.not z
xorb P.False z = z
two :: forall . (?imp :: ()) => Nat
two = S (S O)
this :: forall . (?imp :: ()) => Tree -> Tree
this x2 = x2
tailadd :: forall . (?imp :: ()) => Nat -> Nat -> Nat
tailadd O y2 = y2
tailadd (S n0) y2 = tailadd n0 (S y2)
tailaddmul :: forall . (?imp :: ()) => Nat -> Nat -> Nat -> Nat
tailaddmul x3 O z2 = x3
tailaddmul x3 (S n02) z2 = tailaddmul (tailadd z2 x3) n02 z2
tailmul :: forall . (?imp :: ()) => Nat -> Nat -> Nat
tailmul x4 y3 = tailaddmul O x4 y3
tright :: forall . (?imp :: ()) => Triple -> Tree
tright (Mktriple y4 z3 tright0) = tright0
tleft :: forall . (?imp :: ()) => Triple -> Tree
tleft (Mktriple tleft0 y5 z4) = tleft0
tin :: forall . (?imp :: ()) => Triple -> P.Bool
tin (Mktriple y6 tin0 z5) = tin0
succ0 :: forall . (?imp :: ()) => Positive -> Positive
succ0 (XI p) = XO (succ0 p)
succ0 (XO q) = XI q
succ0 XH = XO XH
succ :: forall . (?imp :: ()) => Uint -> Uint
succ Nil2 = D1 Nil2
succ (D0 d0) = D1 d0
succ (D1 d02) = D2 d02
succ (D2 d03) = D3 d03
succ (D3 d04) = D4 d04
succ (D4 d05) = D5 d05
succ (D5 d06) = D6 d06
succ (D6 d07) = D7 d07
succ (D7 d08) = D8 d08
succ (D8 d09) = D9 d09
succ (D9 d010) = D0 (succ d010)
tolittleuint :: forall . (?imp :: ()) => Nat -> Uint -> Uint
tolittleuint O y7 = y7
tolittleuint (S n03) y7 = tolittleuint n03 (succ y7)
subsetrcompare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
subsetrcompare10 O O = Eq2
subsetrcompare10 O (S z6) = Lt
subsetrcompare10 (S m) O = Gt
subsetrcompare10 (S m) (S o) = subsetrcompare10 m o
subsetlcompare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
subsetlcompare10 O O = Eq2
subsetlcompare10 O (S z7) = Lt
subsetlcompare10 (S n2) O = Gt
subsetlcompare10 (S n2) (S m2) = subsetlcompare10 n2 m2
subsetcompare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
subsetcompare10 O O = Eq2
subsetcompare10 O (S z8) = Lt
subsetcompare10 (S n3) O = Gt
subsetcompare10 (S n3) (S m3) = subsetcompare10 n3 m3
sub :: forall . (?imp :: ()) => Nat -> Nat -> Nat
sub O y8 = O
sub (S i) O = S i
sub (S i) (S l) = sub i l
sqrtiter ::
  forall . (?imp :: ()) => Nat -> Nat -> Nat -> Nat -> Nat
sqrtiter O y9 z9 x22 = y9
sqrtiter (S j) y9 z9 O = sqrtiter j (S y9) (S (S z9)) (S (S z9))
sqrtiter (S j) y9 z9 (S r) = sqrtiter j y9 z9 r
sqrt :: forall . (?imp :: ()) => Nat -> Nat
sqrt x5 = sqrtiter x5 O O O
splitcompare10 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
splitcompare10 O O = Eq2
splitcompare10 O (S z10) = Lt
splitcompare10 (S n4) O = Gt
splitcompare10 (S n4) (S m4) = splitcompare10 n4 m4
snd ::
  forall a1 a22 .
    (QC.Arbitrary a1, F.Enumerable a1, P.Ord a1, QC.Arbitrary a22,
     F.Enumerable a22, P.Ord a22, ?imp :: ()) =>
      Pair a1 a22 -> a22
snd (Pair2 y10 z11) = z11
revapp :: forall . (?imp :: ()) => Uint -> Uint -> Uint
revapp Nil2 y11 = y11
revapp (D0 d011) y11 = revapp d011 (D0 y11)
revapp (D1 d022) y11 = revapp d022 (D1 y11)
revapp (D2 d032) y11 = revapp d032 (D2 y11)
revapp (D3 d042) y11 = revapp d042 (D3 y11)
revapp (D4 d052) y11 = revapp d052 (D4 y11)
revapp (D5 d062) y11 = revapp d062 (D5 y11)
revapp (D6 d072) y11 = revapp d072 (D6 y11)
revapp (D7 d082) y11 = revapp d082 (D7 y11)
revapp (D8 d092) y11 = revapp d092 (D8 y11)
revapp (D9 d0102) y11 = revapp d0102 (D9 y11)
revelementsaux ::
  forall . (?imp :: ()) => List Nat -> Tree -> List Nat
revelementsaux x6 Leaf = x6
revelementsaux x6 (Node z12 l2 x23 r2) =
  revelementsaux (Cons x23 (revelementsaux x6 l2)) r2
revelements :: forall . (?imp :: ()) => Tree -> List Nat
revelements x7 = revelementsaux (Nil :: List Nat) x7
rev :: forall . (?imp :: ()) => Uint -> Uint
rev x8 = revapp x8 Nil2
touint :: forall . (?imp :: ()) => Nat -> Uint
touint x9 = rev (tolittleuint x9 (D0 Nil2))
toint :: forall . (?imp :: ()) => Nat -> Int2
toint x10 = Pos (touint x10)
removecompare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
removecompare10 O O = Eq2
removecompare10 O (S z13) = Lt
removecompare10 (S n5) O = Gt
removecompare10 (S n5) (S m5) = removecompare10 n5 m5
preddouble :: forall . (?imp :: ()) => Positive -> Positive
preddouble (XI p2) = XI (XO p2)
preddouble (XO q2) = XI (preddouble q2)
preddouble XH = XH
preddouble0 :: forall . (?imp :: ()) => Z -> Z
preddouble0 Z0 = Zneg XH
preddouble0 (Zpos p3) = Zpos (preddouble p3)
preddouble0 (Zneg q3) = Zneg (XI q3)
succdouble :: forall . (?imp :: ()) => Z -> Z
succdouble Z0 = Zpos XH
succdouble (Zpos p4) = Zpos (XI p4)
succdouble (Zneg q4) = Zneg (preddouble q4)
pred :: forall . (?imp :: ()) => Nat -> Nat
pred O = O
pred (S u) = u
one :: forall . (?imp :: ()) => Nat
one = S O
ofuintacc :: forall . (?imp :: ()) => Uint -> Nat -> Nat
ofuintacc Nil2 y12 = y12
ofuintacc (D0 d012) y12 =
  ofuintacc
    d012 (tailmul (S (S (S (S (S (S (S (S (S (S O)))))))))) y12)
ofuintacc (D1 d023) y12 =
  ofuintacc
    d023 (S (tailmul (S (S (S (S (S (S (S (S (S (S O)))))))))) y12))
ofuintacc (D2 d033) y12 =
  ofuintacc
    d033
    (S (S (tailmul (S (S (S (S (S (S (S (S (S (S O)))))))))) y12)))
ofuintacc (D3 d043) y12 =
  ofuintacc
    d043
    (S (S (S (tailmul (S (S (S (S (S (S (S (S (S (S O)))))))))) y12))))
ofuintacc (D4 d053) y12 =
  ofuintacc
    d053
    (S (S (S (S (tailmul
                   (S (S (S (S (S (S (S (S (S (S O)))))))))) y12)))))
ofuintacc (D5 d063) y12 =
  ofuintacc
    d063
    (S (S (S (S (S (tailmul
                      (S (S (S (S (S (S (S (S (S (S O)))))))))) y12))))))
ofuintacc (D6 d073) y12 =
  ofuintacc
    d073
    (S (S (S (S (S (S (tailmul
                         (S (S (S (S (S (S (S (S (S (S O)))))))))) y12)))))))
ofuintacc (D7 d083) y12 =
  ofuintacc
    d083
    (S (S (S (S (S (S (S (tailmul
                            (S (S (S (S (S (S (S (S (S (S O)))))))))) y12))))))))
ofuintacc (D8 d093) y12 =
  ofuintacc
    d093
    (S (S (S (S (S (S (S (S (tailmul
                               (S (S (S (S (S (S (S (S (S (S O)))))))))) y12)))))))))
ofuintacc (D9 d0103) y12 =
  ofuintacc
    d0103
    (S (S (S (S (S (S (S (S (S (tailmul
                                  (S (S (S (S (S (S (S (S (S (S O)))))))))) y12))))))))))
ofuint :: forall . (?imp :: ()) => Uint -> Nat
ofuint x11 = ofuintacc x11 O
nzhead :: forall . (?imp :: ()) => Uint -> Uint
nzhead x12 =
  case x12 of
    D0 d013 -> nzhead d013
    _ -> x12
unorm :: forall . (?imp :: ()) => Uint -> Uint
unorm x13 =
  let wild = nzhead x13
    in case wild of
         Nil2 -> D0 Nil2
         _ -> wild
norm :: forall . (?imp :: ()) => Int2 -> Int2
norm (Pos d014) = Pos (unorm d014)
norm (Neg d024) =
  let wild1 = nzhead d024
    in case wild1 of
         Nil2 -> Pos (D0 Nil2)
         _ -> Neg wild1
ofint :: forall . (?imp :: ()) => Int2 -> Maybe Nat
ofint x14 =
  case norm x14 of
    Pos v -> Just (ofuint v)
    Neg y13 -> Nothing :: Maybe Nat
natrect ::
  forall a12 .
    (QC.Arbitrary a12, F.Enumerable a12, P.Ord a12, ?imp :: ()) =>
      a12 -> (Nat -> a12 -> a12) -> Nat -> a12
natrect x15 y14 O = x15
natrect x15 y14 (S n04) = P.id (P.id y14 n04) (natrect x15 y14 n04)
minelt :: forall . (?imp :: ()) => Tree -> Maybe Nat
minelt Leaf = Nothing :: Maybe Nat
minelt (Node y15 Leaf z14 x24) = Just z14
minelt (Node y15 (Node x32 x42 x52 x62) z14 x24) =
  minelt (Node x32 x42 x52 x62)
min1 :: forall . (?imp :: ()) => Nat -> Nat -> Nat
min1 O y16 = O
min1 (S n6) O = O
min1 (S n6) (S m6) = S (min1 n6 m6)
min0 :: forall . (?imp :: ()) => Nat -> Nat -> Nat
min0 O y17 = O
min0 (S n7) O = O
min0 (S n7) (S m7) = S (min0 n7 m7)
min :: forall . (?imp :: ()) => Nat -> Nat -> Nat
min O y18 = O
min (S n8) O = O
min (S n8) (S m8) = S (min n8 m8)
mindepth :: forall . (?imp :: ()) => Tree -> Nat
mindepth Leaf = O
mindepth (Node y19 l3 z15 r3) = S (min (mindepth l3) (mindepth r3))
memcompare10 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
memcompare10 O O = Eq2
memcompare10 O (S z16) = Lt
memcompare10 (S n9) O = Gt
memcompare10 (S n9) (S m9) = memcompare10 n9 m9
mem :: forall . (?imp :: ()) => Nat -> Tree -> P.Bool
mem x16 Leaf = P.False
mem x16 (Node z17 l4 k2 r4) =
  case memcompare10 x16 k2 of
    Eq2 -> P.True
    Lt -> mem x16 l4
    Gt -> mem x16 r4
subsetl ::
  forall . (?imp :: ()) => (Tree -> P.Bool) -> Nat -> Tree -> P.Bool
subsetl x17 y20 Leaf = P.False
subsetl x17 y20 (Node x25 l22 x222 r22) =
  case subsetlcompare10 y20 x222 of
    Eq2 -> P.id x17 l22
    Lt -> subsetl x17 y20 l22
    Gt ->
      case mem y20 r22 of
        P.True -> P.id x17 (Node x25 l22 x222 r22)
        P.False -> P.False
subsetr ::
  forall . (?imp :: ()) => (Tree -> P.Bool) -> Nat -> Tree -> P.Bool
subsetr x18 y21 Leaf = P.False
subsetr x18 y21 (Node x26 l23 x223 r23) =
  case subsetrcompare10 y21 x223 of
    Eq2 -> P.id x18 r23
    Lt ->
      case mem y21 l23 of
        P.True -> P.id x18 (Node x26 l23 x223 r23)
        P.False -> P.False
    Gt -> subsetr x18 y21 r23
subset :: forall . (?imp :: ()) => Tree -> Tree -> P.Bool
subset Leaf y22 = P.True
subset (Node z18 l1 x1 r1) Leaf = P.False
subset (Node z18 l1 x1 r1) (Node x27 l24 x224 r24) =
  case subsetcompare10 x1 x224 of
    Eq2 ->
      case subset l1 l24 of
        P.True -> subset r1 r24
        P.False -> P.False
    Lt ->
      case subsetl (\ x33 -> subset l1 x33) x1 l24 of
        P.True -> subset r1 (Node x27 l24 x224 r24)
        P.False -> P.False
    Gt ->
      case subsetr (\ x43 -> subset r1 x43) x1 r24 of
        P.True -> subset l1 (Node x27 l24 x224 r24)
        P.False -> P.False
maxelt :: forall . (?imp :: ()) => Tree -> Maybe Nat
maxelt Leaf = Nothing :: Maybe Nat
maxelt (Node y23 z19 x28 Leaf) = Just x28
maxelt (Node y23 z19 x28 (Node x34 x44 x53 x63)) =
  maxelt (Node x34 x44 x53 x63)
max2 :: forall . (?imp :: ()) => Nat -> Nat -> Nat
max2 O y24 = y24
max2 (S n10) O = S n10
max2 (S n10) (S m10) = S (max2 n10 m10)
max0 :: forall . (?imp :: ()) => Nat -> Nat -> Nat
max0 O y25 = y25
max0 (S n11) O = S n11
max0 (S n11) (S m11) = S (max0 n11 m11)
max :: forall . (?imp :: ()) => Nat -> Nat -> Nat
max O y26 = y26
max (S n12) O = S n12
max (S n12) (S m12) = S (max n12 m12)
maxdepth :: forall . (?imp :: ()) => Tree -> Nat
maxdepth Leaf = O
maxdepth (Node y27 l5 z20 r5) = S (max (maxdepth l5) (maxdepth r5))
ltbtreecompare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
ltbtreecompare10 O O = Eq2
ltbtreecompare10 O (S z21) = Lt
ltbtreecompare10 (S n13) O = Gt
ltbtreecompare10 (S n13) (S m13) = ltbtreecompare10 n13 m13
ltbtree :: forall . (?imp :: ()) => Nat -> Tree -> P.Bool
ltbtree x19 Leaf = P.True
ltbtree x19 (Node z22 l6 y28 r6) =
  case ltbtreecompare10 x19 y28 of
    Gt -> (ltbtree x19 l6) P.&& (ltbtree x19 r6)
    _ -> P.False
ltdec0compare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
ltdec0compare10 O O = Eq2
ltdec0compare10 O (S z23) = Lt
ltdec0compare10 (S n14) O = Gt
ltdec0compare10 (S n14) (S m14) = ltdec0compare10 n14 m14
ltdeccompare10 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
ltdeccompare10 O O = Eq2
ltdeccompare10 O (S z24) = Lt
ltdeccompare10 (S n15) O = Gt
ltdeccompare10 (S n15) (S m15) = ltdeccompare10 n15 m15
log2iter ::
  forall . (?imp :: ()) => Nat -> Nat -> Nat -> Nat -> Nat
log2iter O y29 z25 x29 = y29
log2iter (S k3) y29 z25 O = log2iter k3 (S y29) (S z25) z25
log2iter (S k3) y29 z25 (S r7) = log2iter k3 y29 (S z25) r7
log2 :: forall . (?imp :: ()) => Nat -> Nat
log2 x20 = log2iter (pred x20) O (S O) O
leb0 :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
leb0 O y30 = P.True
leb0 (S n16) O = P.False
leb0 (S n16) (S m16) = leb0 n16 m16
ltb0 :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
ltb0 x21 y31 = leb0 (S x21) y31
iter ::
  forall a13 .
    (QC.Arbitrary a13, F.Enumerable a13, P.Ord a13, ?imp :: ()) =>
      Nat -> (a13 -> a13) -> a13 -> a13
iter x30 y32 z26 = natrect z26 (\ x210 -> y32) x30
isempty0 :: forall . (?imp :: ()) => Tree -> P.Bool
isempty0 Leaf = P.True
isempty0 (Node y33 z27 x211 x35) = P.False
isempty :: forall . (?imp :: ()) => Tree -> P.Bool
isempty Leaf = P.True
isempty (Node y34 z28 x212 x36) = P.False
iffreflect :: forall . (?imp :: ()) => P.Bool -> Reflect
iffreflect P.True = ReflectT
iffreflect P.False = ReflectF
lebspec0 :: forall . (?imp :: ()) => Nat -> Nat -> Reflect
lebspec0 x31 y35 = iffreflect (leb0 x31 y35)
ltbspec0 :: forall . (?imp :: ()) => Nat -> Nat -> Reflect
ltbspec0 x37 y36 = iffreflect (ltb0 x37 y36)
height :: forall . (?imp :: ()) => Tree -> Z
height Leaf = Z0
height (Node h y37 z29 x213) = h
gtbtreecompare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
gtbtreecompare10 O O = Eq2
gtbtreecompare10 O (S z30) = Lt
gtbtreecompare10 (S n17) O = Gt
gtbtreecompare10 (S n17) (S m17) = gtbtreecompare10 n17 m17
gtbtree :: forall . (?imp :: ()) => Nat -> Tree -> P.Bool
gtbtree x38 Leaf = P.True
gtbtree x38 (Node z31 l7 y210 r8) =
  case gtbtreecompare10 x38 y210 of
    Lt -> (gtbtree x38 l7) P.&& (gtbtree x38 r8)
    _ -> P.False
isok :: forall . (?imp :: ()) => Tree -> P.Bool
isok Leaf = P.True
isok (Node y38 l8 z32 r9) =
  (((isok l8) P.&& (isok r9)) P.&& (ltbtree z32 l8)) P.&&
    (gtbtree z32 r9)
fst ::
  forall a14 a23 .
    (QC.Arbitrary a14, F.Enumerable a14, P.Ord a14, QC.Arbitrary a23,
     F.Enumerable a23, P.Ord a23, ?imp :: ()) =>
      Pair a14 a23 -> a14
fst (Pair2 y39 z33) = y39
forall2 ::
  forall . (?imp :: ()) => (Nat -> P.Bool) -> Tree -> P.Bool
forall2 x39 Leaf = P.True
forall2 x39 (Node z34 l9 x214 r10) =
  case P.id x39 x214 of
    P.True ->
      case forall2 x39 l9 of
        P.True -> forall2 x39 r10
        P.False -> P.False
    P.False -> P.False
fold ::
  forall a15 .
    (QC.Arbitrary a15, F.Enumerable a15, P.Ord a15, ?imp :: ()) =>
      (Nat -> a15 -> a15) -> Tree -> a15 -> a15
fold x40 Leaf z35 = z35
fold x40 (Node x215 l10 x310 r11) z35 =
  fold x40 r11 (P.id (P.id x40 x310) (fold x40 l10 z35))
exists ::
  forall . (?imp :: ()) => (Nat -> P.Bool) -> Tree -> P.Bool
exists x41 Leaf = P.False
exists x41 (Node z36 l11 x216 r12) =
  case P.id x41 x216 of
    P.True -> P.True
    P.False ->
      case exists x41 l11 of
        P.True -> P.True
        P.False -> exists x41 r12
even :: forall . (?imp :: ()) => Nat -> P.Bool
even O = P.True
even (S O) = P.False
even (S (S n18)) = even n18
odd :: forall . (?imp :: ()) => Nat -> P.Bool
odd x45 = P.not (even x45)
eqb :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
eqb O O = P.True
eqb O (S z37) = P.False
eqb (S n19) O = P.False
eqb (S n19) (S m18) = eqb n19 m18
eqbspec :: forall . (?imp :: ()) => Nat -> Nat -> Reflect
eqbspec x46 y40 = iffreflect (eqb x46 y40)
eqrect ::
  forall a16 a24 .
    (QC.Arbitrary a16, F.Enumerable a16, P.Ord a16, QC.Arbitrary a24,
     F.Enumerable a24, P.Ord a24, ?imp :: ()) =>
      a16 -> a24 -> a16 -> a24
eqrect x47 y41 z38 = y41
eqdec :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
eqdec x48 y42 =
  P.id
    (natrect
       (\ m19 ->
          case m19 of
            O -> P.True
            S z39 -> P.False)
       (\ x217 ->
          \ iHn ->
            \ n20 ->
              case n20 of
                O -> P.False
                S m0 -> P.id iHn m0)
       x48)
    y42
elementsaux ::
  forall . (?imp :: ()) => List Nat -> Tree -> List Nat
elementsaux x49 Leaf = x49
elementsaux x49 (Node z40 l12 x218 r13) =
  elementsaux (Cons x218 (elementsaux x49 r13)) l12
elements :: forall . (?imp :: ()) => Tree -> List Nat
elements x50 = elementsaux (Nil :: List Nat) x50
double :: forall . (?imp :: ()) => Z -> Z
double Z0 = Z0
double (Zpos p5) = Zpos (XO p5)
double (Zneg q5) = Zneg (XO q5)
possub :: forall . (?imp :: ()) => Positive -> Positive -> Z
possub (XI p6) (XI q6) = double (possub p6 q6)
possub (XI p6) (XO r14) = succdouble (possub p6 r14)
possub (XI p6) XH = Zpos (XO p6)
possub (XO p22) (XI q22) = preddouble0 (possub p22 q22)
possub (XO p22) (XO q32) = double (possub p22 q32)
possub (XO p22) XH = Zpos (preddouble p22)
possub XH (XI q42) = Zneg (XO q42)
possub XH (XO q52) = Zneg (preddouble q52)
possub XH XH = Z0
divmod ::
  forall . (?imp :: ()) => Nat -> Nat -> Nat -> Nat -> Pair Nat Nat
divmod O y43 z41 x219 = Pair2 z41 x219
divmod (S x51) y43 z41 O = divmod x51 y43 (S z41) y43
divmod (S x51) y43 z41 (S w) = divmod x51 y43 z41 w
gcd :: forall . (?imp :: ()) => Nat -> Nat -> Nat
gcd O y44 = y44
gcd (S a3) y44 = gcd (sub a3 (snd (divmod y44 a3 O a3))) (S a3)
modulo :: forall . (?imp :: ()) => Nat -> Nat -> Nat
modulo x54 O = O
modulo x54 (S y45) = sub y45 (snd (divmod x54 y45 O y45))
div22 :: forall . (?imp :: ()) => Nat -> Nat
div22 O = O
div22 (S O) = O
div22 (S (S n21)) = S (div22 n21)
shiftr :: forall . (?imp :: ()) => Nat -> Nat -> Nat
shiftr x55 y46 = natrect x55 (\ z42 -> \ x220 -> div22 x220) y46
testbit :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
testbit x56 O = odd x56
testbit x56 (S n05) = testbit (div22 x56) n05
div2 :: forall . (?imp :: ()) => Nat -> Nat -> Nat
div2 x57 O = O
div2 x57 (S y47) = fst (divmod x57 y47 O y47)
cons2 ::
  forall . (?imp :: ()) => Tree -> Enumeration -> Enumeration
cons2 Leaf y48 = y48
cons2 (Node z43 l13 x221 r15) y48 = cons2 l13 (More x221 r15 y48)
comparemorecompare10 ::
  forall . (?imp :: ()) => Nat -> Nat -> Comparison
comparemorecompare10 O O = Eq2
comparemorecompare10 O (S z44) = Lt
comparemorecompare10 (S n22) O = Gt
comparemorecompare10 (S n22) (S m20) = comparemorecompare10 n22 m20
comparemore ::
  forall .
    (?imp :: ()) =>
      Nat -> (Enumeration -> Comparison) -> Enumeration -> Comparison
comparemore x58 y49 End = Gt
comparemore x58 y49 (More x225 r25 e3) =
  let wild12 = comparemorecompare10 x58 x225
    in case wild12 of
         Eq2 -> P.id y49 (cons2 r25 e3)
         _ -> wild12
compareend :: forall . (?imp :: ()) => Enumeration -> Comparison
compareend End = Eq2
compareend (More y50 z45 x226) = Lt
comparecont0 ::
  forall .
    (?imp :: ()) =>
      Tree -> (Enumeration -> Comparison) -> Enumeration -> Comparison
comparecont0 Leaf y51 z46 = P.id y51 z46
comparecont0 (Node x227 l14 x110 r16) y51 z46 =
  comparecont0
    l14
    (\ x311 ->
       case x311 of
         End -> Gt
         More x228 r26 e32 ->
           let wild13 = comparemorecompare10 x110 x228
             in case wild13 of
                  Eq2 -> comparecont0 r16 y51 (cons2 r26 e32)
                  _ -> wild13)
    z46
comparecont ::
  forall .
    (?imp :: ()) => Comparison -> Positive -> Positive -> Comparison
comparecont x59 (XI p7) (XI q7) = comparecont x59 p7 q7
comparecont x59 (XI p7) (XO r17) = comparecont Gt p7 r17
comparecont x59 (XI p7) XH = Gt
comparecont x59 (XO p23) (XI q23) = comparecont Lt p23 q23
comparecont x59 (XO p23) (XO q33) = comparecont x59 p23 q33
comparecont x59 (XO p23) XH = Gt
comparecont x59 XH z47 =
  case z47 of
    XH -> x59
    _ -> Lt
compareSpec2Type ::
  forall . (?imp :: ()) => Comparison -> CompareSpecT
compareSpec2Type Eq2 = CompEqT
compareSpec2Type Lt = CompLtT
compareSpec2Type Gt = CompGtT
ltdec :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
ltdec x60 y52 =
  case compareSpec2Type (ltdeccompare10 x60 y52) of
    CompLtT -> P.True
    _ -> P.False
ltdec0 :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
ltdec0 x61 y53 =
  case compareSpec2Type (ltdec0compare10 x61 y53) of
    CompLtT -> P.True
    _ -> P.False
compare8 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
compare8 O O = Eq2
compare8 O (S z48) = Lt
compare8 (S n23) O = Gt
compare8 (S n23) (S m21) = compare8 n23 m21
compare7 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
compare7 O O = Eq2
compare7 O (S z49) = Lt
compare7 (S n24) O = Gt
compare7 (S n24) (S m22) = compare7 n24 m22
compare6 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
compare6 O O = Eq2
compare6 O (S z50) = Lt
compare6 (S n25) O = Gt
compare6 (S n25) (S m23) = compare6 n25 m23
compare5 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
compare5 O O = Eq2
compare5 O (S z51) = Lt
compare5 (S n26) O = Gt
compare5 (S n26) (S m24) = compare5 n26 m24
compare4 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
compare4 O O = Eq2
compare4 O (S z52) = Lt
compare4 (S n27) O = Gt
compare4 (S n27) (S m25) = compare4 n27 m25
compare3 :: forall . (?imp :: ()) => Tree -> Tree -> Comparison
compare3 x64 y54 =
  comparecont0 x64 (\ z53 -> compareend z53) (cons2 y54 End)
equal :: forall . (?imp :: ()) => Tree -> Tree -> P.Bool
equal x65 y55 =
  case compare3 x65 y55 of
    Eq2 -> P.True
    _ -> P.False
compare2 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
compare2 O O = Eq2
compare2 O (S z54) = Lt
compare2 (S n28) O = Gt
compare2 (S n28) (S m26) = compare2 n28 m26
log2up :: forall . (?imp :: ()) => Nat -> Nat
log2up x66 =
  case compare2 (S O) x66 of
    Lt -> S (log2 (pred x66))
    _ -> O
sqrtup :: forall . (?imp :: ()) => Nat -> Nat
sqrtup x67 =
  case compare2 O x67 of
    Lt -> S (sqrt (pred x67))
    _ -> O
compare0 ::
  forall . (?imp :: ()) => Positive -> Positive -> Comparison
compare0 x68 y56 = comparecont Eq2 x68 y56
compare1 :: forall . (?imp :: ()) => Z -> Z -> Comparison
compare1 Z0 Z0 = Eq2
compare1 Z0 (Zpos z55) = Lt
compare1 Z0 (Zneg x229) = Gt
compare1 (Zpos x69) y57 =
  case y57 of
    Zpos y58 -> compare0 x69 y58
    _ -> Gt
compare1 (Zneg x230) y57 =
  case y57 of
    Zneg y211 ->
      case compare0 x230 y211 of
        Eq2 -> Eq2
        Lt -> Gt
        Gt -> Lt
    _ -> Lt
leb :: forall . (?imp :: ()) => Z -> Z -> P.Bool
leb x70 y59 =
  case compare1 x70 y59 of
    Gt -> P.False
    _ -> P.True
ltb :: forall . (?imp :: ()) => Z -> Z -> P.Bool
ltb x71 y60 =
  case compare1 x71 y60 of
    Lt -> P.True
    _ -> P.False
max1 :: forall . (?imp :: ()) => Z -> Z -> Z
max1 x72 y61 =
  case compare1 x72 y61 of
    Lt -> y61
    _ -> x72
compare :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
compare O O = Eq2
compare O (S z56) = Lt
compare (S n29) O = Gt
compare (S n29) (S m27) = compare n29 m27
maxcasestrong ::
  forall a17 .
    (QC.Arbitrary a17, F.Enumerable a17, P.Ord a17, ?imp :: ()) =>
      Nat -> Nat -> (Nat -> Nat -> a17 -> a17) -> a17 -> a17 -> a17
maxcasestrong x73 y62 z57 x231 x312 =
  case compareSpec2Type (compare x73 y62) of
    CompGtT -> P.id (P.id (P.id z57 x73) (max0 x73 y62)) x231
    _ -> P.id (P.id (P.id z57 y62) (max0 x73 y62)) x312
maxcasestrong0 ::
  forall a18 .
    (QC.Arbitrary a18, F.Enumerable a18, P.Ord a18, ?imp :: ()) =>
      Nat -> Nat -> a18 -> a18 -> a18
maxcasestrong0 x74 y63 z58 x232 =
  maxcasestrong x74 y63 (\ x313 -> \ x410 -> \ x111 -> x111) z58 x232
maxdec :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
maxdec x75 y64 =
  maxcasestrong
    x75 y64 (\ z59 -> \ x233 -> \ h0 -> h0) P.True P.False
mincasestrong ::
  forall a19 .
    (QC.Arbitrary a19, F.Enumerable a19, P.Ord a19, ?imp :: ()) =>
      Nat -> Nat -> (Nat -> Nat -> a19 -> a19) -> a19 -> a19 -> a19
mincasestrong x76 y65 z60 x234 x314 =
  case compareSpec2Type (compare x76 y65) of
    CompGtT -> P.id (P.id (P.id z60 y65) (min0 x76 y65)) x314
    _ -> P.id (P.id (P.id z60 x76) (min0 x76 y65)) x234
mincasestrong0 ::
  forall a110 .
    (QC.Arbitrary a110, F.Enumerable a110, P.Ord a110, ?imp :: ()) =>
      Nat -> Nat -> a110 -> a110 -> a110
mincasestrong0 x77 y66 z61 x235 =
  mincasestrong x77 y66 (\ x315 -> \ x411 -> \ x112 -> x112) z61 x235
mindec :: forall . (?imp :: ()) => Nat -> Nat -> P.Bool
mindec x78 y67 =
  mincasestrong
    x78 y67 (\ z62 -> \ x236 -> \ h02 -> h02) P.True P.False
compSpec2Type ::
  forall a111 .
    (QC.Arbitrary a111, F.Enumerable a111, P.Ord a111, ?imp :: ()) =>
      a111 -> a111 -> Comparison -> CompareSpecT
compSpec2Type x79 y68 z63 = compareSpec2Type z63
compOpp :: forall . (?imp :: ()) => Comparison -> Comparison
compOpp Eq2 = Eq2
compOpp Lt = Gt
compOpp Gt = Lt
b2n :: forall . (?imp :: ()) => P.Bool -> Nat
b2n P.True = S O
b2n P.False = O
app ::
  forall a112 .
    (QC.Arbitrary a112, F.Enumerable a112, P.Ord a112, ?imp :: ()) =>
      List a112 -> List a112 -> List a112
app Nil y69 = y69
app (Cons b2 l15) y69 = Cons b2 (app l15 y69)
flattene :: forall . (?imp :: ()) => Enumeration -> List Nat
flattene End = Nil :: List Nat
flattene (More y70 t r18) =
  Cons y70 (app (elements t) (flattene r18))
andrect ::
  forall a113 .
    (QC.Arbitrary a113, F.Enumerable a113, P.Ord a113, ?imp :: ()) =>
      a113 -> a113
andrect x80 = x80
andrec ::
  forall a114 .
    (QC.Arbitrary a114, F.Enumerable a114, P.Ord a114, ?imp :: ()) =>
      a114 -> a114
andrec x81 = x81
add4compare10 :: forall . (?imp :: ()) => Nat -> Nat -> Comparison
add4compare10 O O = Eq2
add4compare10 O (S z64) = Lt
add4compare10 (S n30) O = Gt
add4compare10 (S n30) (S m28) = add4compare10 n30 m28
add2 :: forall . (?imp :: ()) => Nat -> Nat -> Nat
add2 O y71 = y71
add2 (S p8) y71 = S (add2 p8 y71)
double0 :: forall . (?imp :: ()) => Nat -> Nat
double0 x82 = add2 x82 x82
ones :: forall . (?imp :: ()) => Nat -> Nat
ones x83 = pred (natrect (S O) (\ y72 -> \ z65 -> double0 z65) x83)
shiftl :: forall . (?imp :: ()) => Nat -> Nat -> Nat
shiftl x84 y73 = natrect x84 (\ z66 -> \ x237 -> double0 x237) y73
mul :: forall . (?imp :: ()) => Nat -> Nat -> Nat
mul O y74 = O
mul (S p9) y74 = add2 y74 (mul p9 y74)
bitwise ::
  forall .
    (?imp :: ()) =>
      (P.Bool -> P.Bool -> P.Bool) -> Nat -> Nat -> Nat -> Nat
bitwise x85 O z67 x238 = O
bitwise x85 (S n31) z67 x238 =
  case P.id (P.id x85 (odd z67)) (odd x238) of
    P.True ->
      add2
        (S O) (mul (S (S O)) (bitwise x85 n31 (div22 z67) (div22 x238)))
    P.False ->
      add2 O (mul (S (S O)) (bitwise x85 n31 (div22 z67) (div22 x238)))
land :: forall . (?imp :: ()) => Nat -> Nat -> Nat
land x86 y75 =
  bitwise (\ z68 -> \ x239 -> z68 P.&& x239) x86 x86 y75
ldiff :: forall . (?imp :: ()) => Nat -> Nat -> Nat
ldiff x87 y76 =
  bitwise (\ b0 -> \ b3 -> b0 P.&& (P.not b3)) x87 x87 y76
clearbit :: forall . (?imp :: ()) => Nat -> Nat -> Nat
clearbit x88 y77 =
  ldiff x88 (natrect (S O) (\ z69 -> \ x240 -> double0 x240) y77)
lor :: forall . (?imp :: ()) => Nat -> Nat -> Nat
lor x89 y78 =
  bitwise (\ z70 -> \ x241 -> z70 P.|| x241) (max2 x89 y78) x89 y78
setbit :: forall . (?imp :: ()) => Nat -> Nat -> Nat
setbit x90 y79 =
  lor x90 (natrect (S O) (\ z71 -> \ x242 -> double0 x242) y79)
lxor :: forall . (?imp :: ()) => Nat -> Nat -> Nat
lxor x91 y80 =
  bitwise (\ z72 -> \ x243 -> xorb z72 x243) (max2 x91 y80) x91 y80
lnot :: forall . (?imp :: ()) => Nat -> Nat -> Nat
lnot x92 y81 = lxor x92 (ones y81)
lcm :: forall . (?imp :: ()) => Nat -> Nat -> Nat
lcm x93 y82 = mul x93 (div2 y82 (gcd x93 y82))
pow :: forall . (?imp :: ()) => Nat -> Nat -> Nat
pow x94 O = S O
pow x94 (S m02) = mul x94 (pow x94 m02)
square :: forall . (?imp :: ()) => Nat -> Nat
square x95 = mul x95 x95
add0 :: forall . (?imp :: ()) => Positive -> Positive -> Positive
add0 (XI p10) (XI q8) = XO (addcarry p10 q8)
add0 (XI p10) (XO r19) = XI (add0 p10 r19)
add0 (XI p10) XH = XO (succ0 p10)
add0 (XO p24) (XI q24) = XI (add0 p24 q24)
add0 (XO p24) (XO q34) = XO (add0 p24 q34)
add0 (XO p24) XH = XI p24
add0 XH (XI q43) = XO (succ0 q43)
add0 XH (XO q53) = XI q53
add0 XH XH = XO XH
addcarry ::
  forall . (?imp :: ()) => Positive -> Positive -> Positive
addcarry (XI p11) (XI q9) = XI (addcarry p11 q9)
addcarry (XI p11) (XO r20) = XO (addcarry p11 r20)
addcarry (XI p11) XH = XI (succ0 p11)
addcarry (XO p25) (XI q25) = XO (addcarry p25 q25)
addcarry (XO p25) (XO q35) = XI (add0 p25 q35)
addcarry (XO p25) XH = XO (succ0 p25)
addcarry XH (XI q44) = XI (succ0 q44)
addcarry XH (XO q54) = XO (succ0 q54)
addcarry XH XH = XI XH
add1 :: forall . (?imp :: ()) => Z -> Z -> Z
add1 Z0 y83 = y83
add1 (Zpos x96) Z0 = Zpos x96
add1 (Zpos x96) (Zpos y84) = Zpos (add0 x96 y84)
add1 (Zpos x96) (Zneg y212) = possub x96 y212
add1 (Zneg x244) Z0 = Zneg x244
add1 (Zneg x244) (Zpos y310) = possub y310 x244
add1 (Zneg x244) (Zneg y410) = Zneg (add0 x244 y410)
add :: forall . (?imp :: ()) => Nat -> Nat -> Nat
add O y85 = y85
add (S p12) y85 = S (add p12 y85)
cardinal :: forall . (?imp :: ()) => Tree -> Nat
cardinal Leaf = O
cardinal (Node y86 l16 z73 r21) =
  S (add (cardinal l16) (cardinal r21))
x245 :: forall . (?imp :: ()) => Z
x245 = Zpos (XO XH)
x113 :: forall . (?imp :: ()) => Z
x113 = Zpos XH
create :: forall . (?imp :: ()) => Tree -> Nat -> Tree -> Tree
create x97 y87 z74 =
  Node (add1 (max1 (height x97) (height z74)) x113) x97 y87 z74
bal :: forall . (?imp :: ()) => Tree -> Nat -> Tree -> Tree
bal x98 y88 z75 =
  let hl = height x98
    in let hr = height z75
         in case ltb (add1 hr x245) hl of
              P.True ->
                case x98 of
                  Leaf -> Node (add1 (max1 Z0 (height z75)) x113) Leaf y88 z75
                  Node x412 ll lx lr ->
                    case leb (height lr) (height ll) of
                      P.True -> create ll lx (create lr y88 z75)
                      P.False ->
                        case lr of
                          Leaf -> create (Node x412 ll lx Leaf) y88 z75
                          Node x510 lrl lrx lrr ->
                            create (create ll lx lrl) lrx (create lrr y88 z75)
              P.False ->
                case ltb (add1 hl x245) hr of
                  P.True ->
                    case z75 of
                      Leaf -> Node (add1 (max1 (height x98) Z0) x113) x98 y88 Leaf
                      Node x246 rl rx rr ->
                        case leb (height rl) (height rr) of
                          P.True -> create (create x98 y88 rl) rx rr
                          P.False ->
                            case rl of
                              Leaf -> create x98 y88 (Node x246 Leaf rx rr)
                              Node x316 rll rlx rlr ->
                                create (create x98 y88 rll) rlx (create rlr rx rr)
                  P.False -> create x98 y88 z75
add4 :: forall . (?imp :: ()) => Nat -> Tree -> Tree
add4 x99 Leaf = Node x113 Leaf x99 Leaf
add4 x99 (Node f l17 z76 r27) =
  case add4compare10 x99 z76 of
    Eq2 -> Node f l17 z76 r27
    Lt -> bal (add4 x99 l17) z76 r27
    Gt -> bal l17 z76 (add4 x99 r27)
removemin ::
  forall . (?imp :: ()) => Tree -> Nat -> Tree -> Pair Tree Nat
removemin Leaf y89 z77 = Pair2 z77 y89
removemin (Node x247 ll2 lx2 lr2) y89 z77 =
  case removemin ll2 lx2 lr2 of
    Pair2 l18 m29 -> Pair2 (bal l18 y89 z77) m29
merge :: forall . (?imp :: ()) => Tree -> Tree -> Tree
merge Leaf y90 = y90
merge (Node z78 x248 x317 x413) Leaf = Node z78 x248 x317 x413
merge (Node z78 x248 x317 x413) (Node x511 l25 x2210 r28) =
  case removemin l25 x2210 r28 of
    Pair2 s2 m30 -> bal (Node z78 x248 x317 x413) m30 s2
remove :: forall . (?imp :: ()) => Nat -> Tree -> Tree
remove x100 Leaf = Leaf
remove x100 (Node z79 l19 y213 r29) =
  case removecompare10 x100 y213 of
    Eq2 -> merge l19 r29
    Lt -> bal (remove x100 l19) y213 r29
    Gt -> bal l19 y213 (remove x100 r29)
joinjoinaux ::
  forall .
    (?imp :: ()) => Z -> Tree -> Tree -> Nat -> Nat -> Tree -> Tree
joinjoinaux lh ll3 lr3 lx3 x101 Leaf =
  add4 x101 (Node lh ll3 lx3 lr3)
joinjoinaux lh ll3 lr3 lx3 x101 (Node rh rl2 rx2 rr2) =
  case ltb (add1 rh x245) lh of
    P.True -> bal ll3 lx3 (join lr3 x101 (Node rh rl2 rx2 rr2))
    P.False ->
      case ltb (add1 lh x245) rh of
        P.True -> bal (joinjoinaux lh ll3 lr3 lx3 x101 rl2) rx2 rr2
        P.False -> create (Node lh ll3 lx3 lr3) x101 (Node rh rl2 rx2 rr2)
join :: forall . (?imp :: ()) => Tree -> Nat -> Tree -> Tree
join Leaf y91 z80 = add4 y91 z80
join (Node lh2 ll4 lx4 lr4) y91 z80 =
  joinjoinaux lh2 ll4 lr4 lx4 y91 z80
concat :: forall . (?imp :: ()) => Tree -> Tree -> Tree
concat Leaf y92 = y92
concat (Node z81 x249 x318 x414) Leaf = Node z81 x249 x318 x414
concat (Node z81 x249 x318 x414) (Node x512 l26 x2211 r210) =
  case removemin l26 x2211 r210 of
    Pair2 s22 m31 -> join (Node z81 x249 x318 x414) m31 s22
filter :: forall . (?imp :: ()) => (Nat -> P.Bool) -> Tree -> Tree
filter x102 Leaf = Leaf
filter x102 (Node z82 l20 x250 r30) =
  let l21 = filter x102 l20
    in let r31 = filter x102 r30
         in case P.id x102 x250 of
              P.True -> join l21 x250 r31
              P.False -> concat l21 r31
partition ::
  forall . (?imp :: ()) => (Nat -> P.Bool) -> Tree -> Pair Tree Tree
partition x103 Leaf = Pair2 Leaf Leaf
partition x103 (Node z83 l27 x251 r32) =
  case partition x103 l27 of
    Pair2 l110 l28 ->
      case partition x103 r32 of
        Pair2 r110 r211 ->
          case P.id x103 x251 of
            P.True -> Pair2 (join l110 x251 r110) (concat l28 r211)
            P.False -> Pair2 (concat l110 r110) (join l28 x251 r211)
partition0 ::
  forall . (?imp :: ()) => (Nat -> P.Bool) -> Tree -> Pair Tree Tree
partition0 x104 y93 =
  let p13 = partition x104 y93 in Pair2 (fst p13) (snd p13)
split :: forall . (?imp :: ()) => Nat -> Tree -> Triple
split x105 Leaf = Mktriple Leaf P.False Leaf
split x105 (Node z84 l29 y214 r33) =
  case splitcompare10 x105 y214 of
    Eq2 -> Mktriple l29 P.True r33
    Lt ->
      case split x105 l29 of
        Mktriple ll5 b4 rl3 -> Mktriple ll5 b4 (join rl3 y214 r33)
    Gt ->
      case split x105 r33 of
        Mktriple rl22 a4 rr3 -> Mktriple (join l29 y214 rl22) a4 rr3
diff :: forall . (?imp :: ()) => Tree -> Tree -> Tree
diff Leaf y94 = Leaf
diff (Node z85 l111 x114 r111) Leaf = Node z85 l111 x114 r111
diff (Node z85 l111 x114 r111) (Node x252 x319 x415 x513) =
  case split x114 (Node x252 x319 x415 x513) of
    Mktriple l210 pres r212 ->
      case pres of
        P.True -> concat (diff l111 l210) (diff r111 r212)
        P.False -> join (diff l111 l210) x114 (diff r111 r212)
inter :: forall . (?imp :: ()) => Tree -> Tree -> Tree
inter Leaf y95 = Leaf
inter (Node z86 l112 x115 r112) Leaf = Leaf
inter (Node z86 l112 x115 r112) (Node x253 x320 x416 x514) =
  case split x115 (Node x253 x320 x416 x514) of
    Mktriple l211 pres2 r213 ->
      case pres2 of
        P.True -> join (inter l112 l211) x115 (inter r112 r213)
        P.False -> concat (inter l112 l211) (inter r112 r213)
union :: forall . (?imp :: ()) => Tree -> Tree -> Tree
union Leaf y96 = y96
union (Node z87 l113 x116 r113) Leaf = Node z87 l113 x116 r113
union (Node z87 l113 x116 r113) (Node x254 x321 x417 x515) =
  case split x116 (Node x254 x321 x417 x515) of
    Mktriple l212 x610 r214 ->
      join (union l113 l212) x116 (union r113 r214)
singleton :: forall . (?imp :: ()) => Nat -> Tree
singleton x106 = Node x113 Leaf x106 Leaf
sig =
  [QS.con
     "."
     ((\ QS.Dict -> (.)) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          QS.A),
   QS.predicate
     "xorb"
     ((\ QS.Dict -> xorb) ::
        QS.Dict (?imp :: ()) -> P.Bool -> P.Bool -> P.Bool),
   QS.con "two" ((\ QS.Dict -> two) :: QS.Dict (?imp :: ()) -> Nat),
   QS.con
     "this"
     ((\ QS.Dict -> this) :: QS.Dict (?imp :: ()) -> Tree -> Tree),
   QS.con
     "tailadd"
     ((\ QS.Dict -> tailadd) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "tailaddmul"
     ((\ QS.Dict -> tailaddmul) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat -> Nat),
   QS.con
     "tailmul"
     ((\ QS.Dict -> tailmul) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "tright"
     ((\ QS.Dict -> tright) :: QS.Dict (?imp :: ()) -> Triple -> Tree),
   QS.con
     "tleft"
     ((\ QS.Dict -> tleft) :: QS.Dict (?imp :: ()) -> Triple -> Tree),
   QS.predicate
     "tin"
     ((\ QS.Dict -> tin) :: QS.Dict (?imp :: ()) -> Triple -> P.Bool),
   QS.con
     "succ0"
     ((\ QS.Dict -> succ0) ::
        QS.Dict (?imp :: ()) -> Positive -> Positive),
   QS.con
     "succ"
     ((\ QS.Dict -> succ) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
   QS.con
     "tolittleuint"
     ((\ QS.Dict -> tolittleuint) ::
        QS.Dict (?imp :: ()) -> Nat -> Uint -> Uint),
   QS.con
     "subsetrcompare10"
     ((\ QS.Dict -> subsetrcompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "subsetlcompare10"
     ((\ QS.Dict -> subsetlcompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "subsetcompare10"
     ((\ QS.Dict -> subsetcompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "sub"
     ((\ QS.Dict -> sub) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "sqrtiter"
     ((\ QS.Dict -> sqrtiter) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat -> Nat -> Nat),
   QS.con
     "sqrt" ((\ QS.Dict -> sqrt) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con
     "splitcompare10"
     ((\ QS.Dict -> splitcompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "snd"
     ((\ QS.Dict -> snd) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A,
           QC.Arbitrary QS.B, F.Enumerable QS.B, P.Ord QS.B, ?imp :: ()) ->
          Pair QS.A QS.B -> QS.B),
   QS.con
     "revapp"
     ((\ QS.Dict -> revapp) ::
        QS.Dict (?imp :: ()) -> Uint -> Uint -> Uint),
   QS.con
     "revelementsaux"
     ((\ QS.Dict -> revelementsaux) ::
        QS.Dict (?imp :: ()) -> List Nat -> Tree -> List Nat),
   QS.con
     "revelements"
     ((\ QS.Dict -> revelements) ::
        QS.Dict (?imp :: ()) -> Tree -> List Nat),
   QS.con
     "rev" ((\ QS.Dict -> rev) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
   QS.con
     "touint"
     ((\ QS.Dict -> touint) :: QS.Dict (?imp :: ()) -> Nat -> Uint),
   QS.con
     "toint"
     ((\ QS.Dict -> toint) :: QS.Dict (?imp :: ()) -> Nat -> Int2),
   QS.con
     "removecompare10"
     ((\ QS.Dict -> removecompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "preddouble"
     ((\ QS.Dict -> preddouble) ::
        QS.Dict (?imp :: ()) -> Positive -> Positive),
   QS.con
     "preddouble0"
     ((\ QS.Dict -> preddouble0) :: QS.Dict (?imp :: ()) -> Z -> Z),
   QS.con
     "succdouble"
     ((\ QS.Dict -> succdouble) :: QS.Dict (?imp :: ()) -> Z -> Z),
   QS.con
     "pred" ((\ QS.Dict -> pred) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con "one" ((\ QS.Dict -> one) :: QS.Dict (?imp :: ()) -> Nat),
   QS.con
     "ofuintacc"
     ((\ QS.Dict -> ofuintacc) ::
        QS.Dict (?imp :: ()) -> Uint -> Nat -> Nat),
   QS.con
     "ofuint"
     ((\ QS.Dict -> ofuint) :: QS.Dict (?imp :: ()) -> Uint -> Nat),
   QS.con
     "nzhead"
     ((\ QS.Dict -> nzhead) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
   QS.con
     "unorm"
     ((\ QS.Dict -> unorm) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
   QS.con
     "norm"
     ((\ QS.Dict -> norm) :: QS.Dict (?imp :: ()) -> Int2 -> Int2),
   QS.con
     "ofint"
     ((\ QS.Dict -> ofint) ::
        QS.Dict (?imp :: ()) -> Int2 -> Maybe Nat),
   QS.con
     "natrect"
     ((\ QS.Dict -> natrect) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          QS.A -> (Nat -> QS.A -> QS.A) -> Nat -> QS.A),
   QS.con
     "minelt"
     ((\ QS.Dict -> minelt) ::
        QS.Dict (?imp :: ()) -> Tree -> Maybe Nat),
   QS.con
     "min1"
     ((\ QS.Dict -> min1) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "min0"
     ((\ QS.Dict -> min0) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "min"
     ((\ QS.Dict -> min) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "mindepth"
     ((\ QS.Dict -> mindepth) :: QS.Dict (?imp :: ()) -> Tree -> Nat),
   QS.con
     "memcompare10"
     ((\ QS.Dict -> memcompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.predicate
     "mem"
     ((\ QS.Dict -> mem) ::
        QS.Dict (?imp :: ()) -> Nat -> Tree -> P.Bool),
   QS.predicate
     "subsetl"
     ((\ QS.Dict -> subsetl) ::
        QS.Dict (?imp :: ()) -> (Tree -> P.Bool) -> Nat -> Tree -> P.Bool),
   QS.predicate
     "subsetr"
     ((\ QS.Dict -> subsetr) ::
        QS.Dict (?imp :: ()) -> (Tree -> P.Bool) -> Nat -> Tree -> P.Bool),
   QS.predicate
     "subset"
     ((\ QS.Dict -> subset) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> P.Bool),
   QS.con
     "maxelt"
     ((\ QS.Dict -> maxelt) ::
        QS.Dict (?imp :: ()) -> Tree -> Maybe Nat),
   QS.con
     "max2"
     ((\ QS.Dict -> max2) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "max0"
     ((\ QS.Dict -> max0) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "max"
     ((\ QS.Dict -> max) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "maxdepth"
     ((\ QS.Dict -> maxdepth) :: QS.Dict (?imp :: ()) -> Tree -> Nat),
   QS.con
     "ltbtreecompare10"
     ((\ QS.Dict -> ltbtreecompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.predicate
     "ltbtree"
     ((\ QS.Dict -> ltbtree) ::
        QS.Dict (?imp :: ()) -> Nat -> Tree -> P.Bool),
   QS.con
     "ltdec0compare10"
     ((\ QS.Dict -> ltdec0compare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "ltdeccompare10"
     ((\ QS.Dict -> ltdeccompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "log2iter"
     ((\ QS.Dict -> log2iter) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat -> Nat -> Nat),
   QS.con
     "log2" ((\ QS.Dict -> log2) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.predicate
     "leb0"
     ((\ QS.Dict -> leb0) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.predicate
     "ltb0"
     ((\ QS.Dict -> ltb0) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.con
     "iter"
     ((\ QS.Dict -> iter) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          Nat -> (QS.A -> QS.A) -> QS.A -> QS.A),
   QS.predicate
     "isempty0"
     ((\ QS.Dict -> isempty0) ::
        QS.Dict (?imp :: ()) -> Tree -> P.Bool),
   QS.predicate
     "isempty"
     ((\ QS.Dict -> isempty) :: QS.Dict (?imp :: ()) -> Tree -> P.Bool),
   QS.con
     "iffreflect"
     ((\ QS.Dict -> iffreflect) ::
        QS.Dict (?imp :: ()) -> P.Bool -> Reflect),
   QS.con
     "lebspec0"
     ((\ QS.Dict -> lebspec0) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Reflect),
   QS.con
     "ltbspec0"
     ((\ QS.Dict -> ltbspec0) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Reflect),
   QS.con
     "height"
     ((\ QS.Dict -> height) :: QS.Dict (?imp :: ()) -> Tree -> Z),
   QS.con
     "gtbtreecompare10"
     ((\ QS.Dict -> gtbtreecompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.predicate
     "gtbtree"
     ((\ QS.Dict -> gtbtree) ::
        QS.Dict (?imp :: ()) -> Nat -> Tree -> P.Bool),
   QS.predicate
     "isok"
     ((\ QS.Dict -> isok) :: QS.Dict (?imp :: ()) -> Tree -> P.Bool),
   QS.con
     "fst"
     ((\ QS.Dict -> fst) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A,
           QC.Arbitrary QS.B, F.Enumerable QS.B, P.Ord QS.B, ?imp :: ()) ->
          Pair QS.A QS.B -> QS.A),
   QS.predicate
     "forall2"
     ((\ QS.Dict -> forall2) ::
        QS.Dict (?imp :: ()) -> (Nat -> P.Bool) -> Tree -> P.Bool),
   QS.con
     "fold"
     ((\ QS.Dict -> fold) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          (Nat -> QS.A -> QS.A) -> Tree -> QS.A -> QS.A),
   QS.predicate
     "exists"
     ((\ QS.Dict -> exists) ::
        QS.Dict (?imp :: ()) -> (Nat -> P.Bool) -> Tree -> P.Bool),
   QS.predicate
     "even"
     ((\ QS.Dict -> even) :: QS.Dict (?imp :: ()) -> Nat -> P.Bool),
   QS.predicate
     "odd"
     ((\ QS.Dict -> odd) :: QS.Dict (?imp :: ()) -> Nat -> P.Bool),
   QS.predicate
     "eqb"
     ((\ QS.Dict -> eqb) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.con
     "eqbspec"
     ((\ QS.Dict -> eqbspec) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Reflect),
   QS.con
     "eqrect"
     ((\ QS.Dict -> eqrect) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A,
           QC.Arbitrary QS.B, F.Enumerable QS.B, P.Ord QS.B, ?imp :: ()) ->
          QS.A -> QS.B -> QS.A -> QS.B),
   QS.predicate
     "eqdec"
     ((\ QS.Dict -> eqdec) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.con
     "elementsaux"
     ((\ QS.Dict -> elementsaux) ::
        QS.Dict (?imp :: ()) -> List Nat -> Tree -> List Nat),
   QS.con
     "elements"
     ((\ QS.Dict -> elements) ::
        QS.Dict (?imp :: ()) -> Tree -> List Nat),
   QS.con
     "double" ((\ QS.Dict -> double) :: QS.Dict (?imp :: ()) -> Z -> Z),
   QS.con
     "possub"
     ((\ QS.Dict -> possub) ::
        QS.Dict (?imp :: ()) -> Positive -> Positive -> Z),
   QS.con
     "divmod"
     ((\ QS.Dict -> divmod) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat -> Nat -> Pair Nat Nat),
   QS.con
     "gcd"
     ((\ QS.Dict -> gcd) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "modulo"
     ((\ QS.Dict -> modulo) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "div22"
     ((\ QS.Dict -> div22) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con
     "shiftr"
     ((\ QS.Dict -> shiftr) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.predicate
     "testbit"
     ((\ QS.Dict -> testbit) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.con
     "div2"
     ((\ QS.Dict -> div2) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "cons2"
     ((\ QS.Dict -> cons2) ::
        QS.Dict (?imp :: ()) -> Tree -> Enumeration -> Enumeration),
   QS.con
     "comparemorecompare10"
     ((\ QS.Dict -> comparemorecompare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "comparemore"
     ((\ QS.Dict -> comparemore) ::
        QS.Dict (?imp :: ()) ->
          Nat -> (Enumeration -> Comparison) -> Enumeration -> Comparison),
   QS.con
     "compareend"
     ((\ QS.Dict -> compareend) ::
        QS.Dict (?imp :: ()) -> Enumeration -> Comparison),
   QS.con
     "comparecont0"
     ((\ QS.Dict -> comparecont0) ::
        QS.Dict (?imp :: ()) ->
          Tree -> (Enumeration -> Comparison) -> Enumeration -> Comparison),
   QS.con
     "comparecont"
     ((\ QS.Dict -> comparecont) ::
        QS.Dict (?imp :: ()) ->
          Comparison -> Positive -> Positive -> Comparison),
   QS.con
     "compareSpec2Type"
     ((\ QS.Dict -> compareSpec2Type) ::
        QS.Dict (?imp :: ()) -> Comparison -> CompareSpecT),
   QS.predicate
     "ltdec"
     ((\ QS.Dict -> ltdec) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.predicate
     "ltdec0"
     ((\ QS.Dict -> ltdec0) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.con
     "compare8"
     ((\ QS.Dict -> compare8) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "compare7"
     ((\ QS.Dict -> compare7) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "compare6"
     ((\ QS.Dict -> compare6) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "compare5"
     ((\ QS.Dict -> compare5) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "compare4"
     ((\ QS.Dict -> compare4) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "compare3"
     ((\ QS.Dict -> compare3) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> Comparison),
   QS.predicate
     "equal"
     ((\ QS.Dict -> equal) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> P.Bool),
   QS.con
     "compare2"
     ((\ QS.Dict -> compare2) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "log2up"
     ((\ QS.Dict -> log2up) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con
     "sqrtup"
     ((\ QS.Dict -> sqrtup) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con
     "compare0"
     ((\ QS.Dict -> compare0) ::
        QS.Dict (?imp :: ()) -> Positive -> Positive -> Comparison),
   QS.con
     "compare1"
     ((\ QS.Dict -> compare1) ::
        QS.Dict (?imp :: ()) -> Z -> Z -> Comparison),
   QS.predicate
     "leb"
     ((\ QS.Dict -> leb) :: QS.Dict (?imp :: ()) -> Z -> Z -> P.Bool),
   QS.predicate
     "ltb"
     ((\ QS.Dict -> ltb) :: QS.Dict (?imp :: ()) -> Z -> Z -> P.Bool),
   QS.con
     "max1"
     ((\ QS.Dict -> max1) :: QS.Dict (?imp :: ()) -> Z -> Z -> Z),
   QS.con
     "compare"
     ((\ QS.Dict -> compare) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "maxcasestrong"
     ((\ QS.Dict -> maxcasestrong) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          Nat ->
            Nat -> (Nat -> Nat -> QS.A -> QS.A) -> QS.A -> QS.A -> QS.A),
   QS.con
     "maxcasestrong0"
     ((\ QS.Dict -> maxcasestrong0) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          Nat -> Nat -> QS.A -> QS.A -> QS.A),
   QS.predicate
     "maxdec"
     ((\ QS.Dict -> maxdec) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.con
     "mincasestrong"
     ((\ QS.Dict -> mincasestrong) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          Nat ->
            Nat -> (Nat -> Nat -> QS.A -> QS.A) -> QS.A -> QS.A -> QS.A),
   QS.con
     "mincasestrong0"
     ((\ QS.Dict -> mincasestrong0) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          Nat -> Nat -> QS.A -> QS.A -> QS.A),
   QS.predicate
     "mindec"
     ((\ QS.Dict -> mindec) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> P.Bool),
   QS.con
     "compSpec2Type"
     ((\ QS.Dict -> compSpec2Type) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          QS.A -> QS.A -> Comparison -> CompareSpecT),
   QS.con
     "compOpp"
     ((\ QS.Dict -> compOpp) ::
        QS.Dict (?imp :: ()) -> Comparison -> Comparison),
   QS.con
     "b2n"
     ((\ QS.Dict -> b2n) :: QS.Dict (?imp :: ()) -> P.Bool -> Nat),
   QS.con
     "app"
     ((\ QS.Dict -> app) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          List QS.A -> List QS.A -> List QS.A),
   QS.con
     "flattene"
     ((\ QS.Dict -> flattene) ::
        QS.Dict (?imp :: ()) -> Enumeration -> List Nat),
   QS.con
     "andrect"
     ((\ QS.Dict -> andrect) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          QS.A -> QS.A),
   QS.con
     "andrec"
     ((\ QS.Dict -> andrec) ::
        QS.Dict
          (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
          QS.A -> QS.A),
   QS.con
     "add4compare10"
     ((\ QS.Dict -> add4compare10) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Comparison),
   QS.con
     "add2"
     ((\ QS.Dict -> add2) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "double0"
     ((\ QS.Dict -> double0) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con
     "ones" ((\ QS.Dict -> ones) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con
     "shiftl"
     ((\ QS.Dict -> shiftl) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "mul"
     ((\ QS.Dict -> mul) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "bitwise"
     ((\ QS.Dict -> bitwise) ::
        QS.Dict (?imp :: ()) ->
          (P.Bool -> P.Bool -> P.Bool) -> Nat -> Nat -> Nat -> Nat),
   QS.con
     "land"
     ((\ QS.Dict -> land) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "ldiff"
     ((\ QS.Dict -> ldiff) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "clearbit"
     ((\ QS.Dict -> clearbit) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "lor"
     ((\ QS.Dict -> lor) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "setbit"
     ((\ QS.Dict -> setbit) ::
        QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "lxor"
     ((\ QS.Dict -> lxor) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "lnot"
     ((\ QS.Dict -> lnot) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "lcm"
     ((\ QS.Dict -> lcm) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "pow"
     ((\ QS.Dict -> pow) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "square"
     ((\ QS.Dict -> square) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
   QS.con
     "add0"
     ((\ QS.Dict -> add0) ::
        QS.Dict (?imp :: ()) -> Positive -> Positive -> Positive),
   QS.con
     "addcarry"
     ((\ QS.Dict -> addcarry) ::
        QS.Dict (?imp :: ()) -> Positive -> Positive -> Positive),
   QS.con
     "add1"
     ((\ QS.Dict -> add1) :: QS.Dict (?imp :: ()) -> Z -> Z -> Z),
   QS.con
     "add"
     ((\ QS.Dict -> add) :: QS.Dict (?imp :: ()) -> Nat -> Nat -> Nat),
   QS.con
     "cardinal"
     ((\ QS.Dict -> cardinal) :: QS.Dict (?imp :: ()) -> Tree -> Nat),
   QS.con "x245" ((\ QS.Dict -> x245) :: QS.Dict (?imp :: ()) -> Z),
   QS.con "x113" ((\ QS.Dict -> x113) :: QS.Dict (?imp :: ()) -> Z),
   QS.con
     "create"
     ((\ QS.Dict -> create) ::
        QS.Dict (?imp :: ()) -> Tree -> Nat -> Tree -> Tree),
   QS.con
     "bal"
     ((\ QS.Dict -> bal) ::
        QS.Dict (?imp :: ()) -> Tree -> Nat -> Tree -> Tree),
   QS.con
     "add4"
     ((\ QS.Dict -> add4) ::
        QS.Dict (?imp :: ()) -> Nat -> Tree -> Tree),
   QS.con
     "removemin"
     ((\ QS.Dict -> removemin) ::
        QS.Dict (?imp :: ()) -> Tree -> Nat -> Tree -> Pair Tree Nat),
   QS.con
     "merge"
     ((\ QS.Dict -> merge) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> Tree),
   QS.con
     "remove"
     ((\ QS.Dict -> remove) ::
        QS.Dict (?imp :: ()) -> Nat -> Tree -> Tree),
   QS.con
     "joinjoinaux"
     ((\ QS.Dict -> joinjoinaux) ::
        QS.Dict (?imp :: ()) ->
          Z -> Tree -> Tree -> Nat -> Nat -> Tree -> Tree),
   QS.con
     "join"
     ((\ QS.Dict -> join) ::
        QS.Dict (?imp :: ()) -> Tree -> Nat -> Tree -> Tree),
   QS.con
     "concat"
     ((\ QS.Dict -> concat) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> Tree),
   QS.con
     "filter"
     ((\ QS.Dict -> filter) ::
        QS.Dict (?imp :: ()) -> (Nat -> P.Bool) -> Tree -> Tree),
   QS.con
     "partition"
     ((\ QS.Dict -> partition) ::
        QS.Dict (?imp :: ()) -> (Nat -> P.Bool) -> Tree -> Pair Tree Tree),
   QS.con
     "partition0"
     ((\ QS.Dict -> partition0) ::
        QS.Dict (?imp :: ()) -> (Nat -> P.Bool) -> Tree -> Pair Tree Tree),
   QS.con
     "split"
     ((\ QS.Dict -> split) ::
        QS.Dict (?imp :: ()) -> Nat -> Tree -> Triple),
   QS.con
     "diff"
     ((\ QS.Dict -> diff) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> Tree),
   QS.con
     "inter"
     ((\ QS.Dict -> inter) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> Tree),
   QS.con
     "union"
     ((\ QS.Dict -> union) ::
        QS.Dict (?imp :: ()) -> Tree -> Tree -> Tree),
   QS.con
     "singleton"
     ((\ QS.Dict -> singleton) :: QS.Dict (?imp :: ()) -> Nat -> Tree),
   QS.background
     [QS.con "False" P.False,
      QS.con "True" P.True,
      QS.con
        "Pair2"
        ((\ QS.Dict -> Pair2) ::
           QS.Dict
             (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A,
              QC.Arbitrary QS.B, F.Enumerable QS.B, P.Ord QS.B, ?imp :: ()) ->
             QS.A -> QS.B -> Pair QS.A QS.B),
      QS.con
        "Nil"
        ((\ QS.Dict -> Nil) ::
           QS.Dict
             (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
             List QS.A),
      QS.con
        "Cons"
        ((\ QS.Dict -> Cons) ::
           QS.Dict
             (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
             QS.A -> List QS.A -> List QS.A),
      QS.con
        "Nil2" ((\ QS.Dict -> Nil2) :: QS.Dict (?imp :: ()) -> Uint),
      QS.con
        "D0" ((\ QS.Dict -> D0) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D1" ((\ QS.Dict -> D1) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D2" ((\ QS.Dict -> D2) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D3" ((\ QS.Dict -> D3) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D4" ((\ QS.Dict -> D4) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D5" ((\ QS.Dict -> D5) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D6" ((\ QS.Dict -> D6) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D7" ((\ QS.Dict -> D7) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D8" ((\ QS.Dict -> D8) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "D9" ((\ QS.Dict -> D9) :: QS.Dict (?imp :: ()) -> Uint -> Uint),
      QS.con
        "ReflectT"
        ((\ QS.Dict -> ReflectT) :: QS.Dict (?imp :: ()) -> Reflect),
      QS.con
        "ReflectF"
        ((\ QS.Dict -> ReflectF) :: QS.Dict (?imp :: ()) -> Reflect),
      QS.con
        "XI"
        ((\ QS.Dict -> XI) ::
           QS.Dict (?imp :: ()) -> Positive -> Positive),
      QS.con
        "XO"
        ((\ QS.Dict -> XO) ::
           QS.Dict (?imp :: ()) -> Positive -> Positive),
      QS.con
        "XH" ((\ QS.Dict -> XH) :: QS.Dict (?imp :: ()) -> Positive),
      QS.con "Z0" ((\ QS.Dict -> Z0) :: QS.Dict (?imp :: ()) -> Z),
      QS.con
        "Zpos"
        ((\ QS.Dict -> Zpos) :: QS.Dict (?imp :: ()) -> Positive -> Z),
      QS.con
        "Zneg"
        ((\ QS.Dict -> Zneg) :: QS.Dict (?imp :: ()) -> Positive -> Z),
      QS.con "O" ((\ QS.Dict -> O) :: QS.Dict (?imp :: ()) -> Nat),
      QS.con
        "S" ((\ QS.Dict -> S) :: QS.Dict (?imp :: ()) -> Nat -> Nat),
      QS.con
        "Leaf" ((\ QS.Dict -> Leaf) :: QS.Dict (?imp :: ()) -> Tree),
      QS.con
        "Node"
        ((\ QS.Dict -> Node) ::
           QS.Dict (?imp :: ()) -> Z -> Tree -> Nat -> Tree -> Tree),
      QS.con
        "Mktriple"
        ((\ QS.Dict -> Mktriple) ::
           QS.Dict (?imp :: ()) -> Tree -> P.Bool -> Tree -> Triple),
      QS.con
        "Nothing"
        ((\ QS.Dict -> Nothing) ::
           QS.Dict
             (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
             Maybe QS.A),
      QS.con
        "Just"
        ((\ QS.Dict -> Just) ::
           QS.Dict
             (QC.Arbitrary QS.A, F.Enumerable QS.A, P.Ord QS.A, ?imp :: ()) ->
             QS.A -> Maybe QS.A),
      QS.con
        "Pos" ((\ QS.Dict -> Pos) :: QS.Dict (?imp :: ()) -> Uint -> Int2),
      QS.con
        "Neg" ((\ QS.Dict -> Neg) :: QS.Dict (?imp :: ()) -> Uint -> Int2),
      QS.con
        "End" ((\ QS.Dict -> End) :: QS.Dict (?imp :: ()) -> Enumeration),
      QS.con
        "More"
        ((\ QS.Dict -> More) ::
           QS.Dict (?imp :: ()) -> Nat -> Tree -> Enumeration -> Enumeration),
      QS.con
        "Eq2" ((\ QS.Dict -> Eq2) :: QS.Dict (?imp :: ()) -> Comparison),
      QS.con
        "Lt" ((\ QS.Dict -> Lt) :: QS.Dict (?imp :: ()) -> Comparison),
      QS.con
        "Gt" ((\ QS.Dict -> Gt) :: QS.Dict (?imp :: ()) -> Comparison),
      QS.con
        "CompEqT"
        ((\ QS.Dict -> CompEqT) :: QS.Dict (?imp :: ()) -> CompareSpecT),
      QS.con
        "CompLtT"
        ((\ QS.Dict -> CompLtT) :: QS.Dict (?imp :: ()) -> CompareSpecT),
      QS.con
        "CompGtT"
        ((\ QS.Dict -> CompGtT) :: QS.Dict (?imp :: ()) -> CompareSpecT)],
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable P.Rational)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable P.Bool)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary P.Rational)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary P.Bool)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary P.Rational)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary P.Bool)),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (P.Ord QS.A, P.Ord QS.B) QS.:- (P.Ord (Pair QS.A QS.B))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (F.Enumerable QS.A, F.Enumerable QS.B) QS.:-
          (F.Enumerable (Pair QS.A QS.B))),
   QS.inst
     ((QS.Sub QS.Dict) :: (P.Ord QS.A) QS.:- (P.Ord (List QS.A))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (F.Enumerable QS.A) QS.:- (F.Enumerable (List QS.A))),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Uint)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Uint)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Reflect)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Reflect)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Positive)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Positive)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Z)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Z)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Nat)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Nat)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Tree)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Tree)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Triple)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Triple)),
   QS.inst
     ((QS.Sub QS.Dict) :: (P.Ord QS.A) QS.:- (P.Ord (Maybe QS.A))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (F.Enumerable QS.A) QS.:- (F.Enumerable (Maybe QS.A))),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Int2)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Int2)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Enumeration)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Enumeration)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord Comparison)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable Comparison)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord CompareSpecT)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable CompareSpecT)),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (QC.Arbitrary QS.A, QC.Arbitrary QS.B, QC.CoArbitrary QS.A,
         QC.CoArbitrary QS.B) QS.:-
          (QC.CoArbitrary (Pair QS.A QS.B))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (T.Typeable QS.A, T.Typeable QS.B) QS.:-
          (T.Typeable (Pair QS.A QS.B))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (QC.Arbitrary QS.A, QC.CoArbitrary QS.A) QS.:-
          (QC.CoArbitrary (List QS.A))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (T.Typeable QS.A) QS.:- (T.Typeable (List QS.A))),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Uint)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Uint)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Reflect)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Reflect)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Positive)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Positive)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Z)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Z)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Nat)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Nat)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Tree)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Tree)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Triple)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Triple)),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (QC.Arbitrary QS.A, QC.CoArbitrary QS.A) QS.:-
          (QC.CoArbitrary (Maybe QS.A))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (T.Typeable QS.A) QS.:- (T.Typeable (Maybe QS.A))),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Int2)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Int2)),
   QS.inst
     ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Enumeration)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Enumeration)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary Comparison)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable Comparison)),
   QS.inst
     ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary CompareSpecT)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable CompareSpecT)),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (F.Enumerable QS.A, F.Enumerable QS.B) QS.:-
          (QC.Arbitrary (Pair QS.A QS.B))),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (F.Enumerable QS.A) QS.:- (QC.Arbitrary (List QS.A))),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Uint)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Reflect)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Positive)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Z)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Nat)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Tree)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Triple)),
   QS.inst
     ((QS.Sub QS.Dict) ::
        (F.Enumerable QS.A) QS.:- (QC.Arbitrary (Maybe QS.A))),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Int2)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Enumeration)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary Comparison)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary CompareSpecT)),
   QS.inst (\ () -> gen),
   QS.withMaxTermSize (5),
   QS.withMaxTestSize (20)]
