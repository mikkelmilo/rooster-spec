module ZArithTest where

import qualified Prelude

data Nat =
   O
 | S Nat

fst :: ((,) a1 a2) -> a1
fst p =
  case p of {
   (,) x _ -> x}

snd :: ((,) a1 a2) -> a2
snd p =
  case p of {
   (,) _ y -> y}

data Comparison =
   Eq
 | Lt
 | Gt

compOpp :: Comparison -> Comparison
compOpp r =
  case r of {
   Eq -> Eq;
   Lt -> Gt;
   Gt -> Lt}

data Uint =
   Nil
 | D0 Uint
 | D1 Uint
 | D2 Uint
 | D3 Uint
 | D4 Uint
 | D5 Uint
 | D6 Uint
 | D7 Uint
 | D8 Uint
 | D9 Uint

data Int =
   Pos Uint
 | Neg Uint

revapp :: Uint -> Uint -> Uint
revapp d d' =
  case d of {
   Nil -> d';
   D0 d0 -> revapp d0 (D0 d');
   D1 d0 -> revapp d0 (D1 d');
   D2 d0 -> revapp d0 (D2 d');
   D3 d0 -> revapp d0 (D3 d');
   D4 d0 -> revapp d0 (D4 d');
   D5 d0 -> revapp d0 (D5 d');
   D6 d0 -> revapp d0 (D6 d');
   D7 d0 -> revapp d0 (D7 d');
   D8 d0 -> revapp d0 (D8 d');
   D9 d0 -> revapp d0 (D9 d')}

rev :: Uint -> Uint
rev d =
  revapp d Nil

double :: Uint -> Uint
double d =
  case d of {
   Nil -> Nil;
   D0 d0 -> D0 (double d0);
   D1 d0 -> D2 (double d0);
   D2 d0 -> D4 (double d0);
   D3 d0 -> D6 (double d0);
   D4 d0 -> D8 (double d0);
   D5 d0 -> D0 (succ_double d0);
   D6 d0 -> D2 (succ_double d0);
   D7 d0 -> D4 (succ_double d0);
   D8 d0 -> D6 (succ_double d0);
   D9 d0 -> D8 (succ_double d0)}

succ_double :: Uint -> Uint
succ_double d =
  case d of {
   Nil -> D1 Nil;
   D0 d0 -> D1 (double d0);
   D1 d0 -> D3 (double d0);
   D2 d0 -> D5 (double d0);
   D3 d0 -> D7 (double d0);
   D4 d0 -> D9 (double d0);
   D5 d0 -> D1 (succ_double d0);
   D6 d0 -> D3 (succ_double d0);
   D7 d0 -> D5 (succ_double d0);
   D8 d0 -> D7 (succ_double d0);
   D9 d0 -> D9 (succ_double d0)}

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

data Positive =
   XI Positive
 | XO Positive
 | XH

data N =
   N0
 | Npos Positive

data Z =
   Z0
 | Zpos Positive
 | Zneg Positive

succ :: Positive -> Positive
succ x =
  case x of {
   XI p -> XO (succ p);
   XO p -> XI p;
   XH -> XO XH}

add0 :: Positive -> Positive -> Positive
add0 x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add0 p q);
     XH -> XO (succ p)};
   XO p ->
    case y of {
     XI q -> XI (add0 p q);
     XO q -> XO (add0 p q);
     XH -> XI p};
   XH -> case y of {
          XI q -> XO (succ q);
          XO q -> XI q;
          XH -> XO XH}}

add_carry :: Positive -> Positive -> Positive
add_carry x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XI (add_carry p q);
     XO q -> XO (add_carry p q);
     XH -> XI (succ p)};
   XO p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add0 p q);
     XH -> XO (succ p)};
   XH -> case y of {
          XI q -> XI (succ q);
          XO q -> XO (succ q);
          XH -> XI XH}}

pred_double :: Positive -> Positive
pred_double x =
  case x of {
   XI p -> XI (XO p);
   XO p -> XI (pred_double p);
   XH -> XH}

pred_N :: Positive -> N
pred_N x =
  case x of {
   XI p -> Npos (XO p);
   XO p -> Npos (pred_double p);
   XH -> N0}

data Mask =
   IsNul
 | IsPos Positive
 | IsNeg

succ_double_mask :: Mask -> Mask
succ_double_mask x =
  case x of {
   IsNul -> IsPos XH;
   IsPos p -> IsPos (XI p);
   IsNeg -> IsNeg}

double_mask :: Mask -> Mask
double_mask x =
  case x of {
   IsPos p -> IsPos (XO p);
   x0 -> x0}

double_pred_mask :: Positive -> Mask
double_pred_mask x =
  case x of {
   XI p -> IsPos (XO (XO p));
   XO p -> IsPos (XO (pred_double p));
   XH -> IsNul}

sub_mask :: Positive -> Positive -> Mask
sub_mask x y =
  case x of {
   XI p ->
    case y of {
     XI q -> double_mask (sub_mask p q);
     XO q -> succ_double_mask (sub_mask p q);
     XH -> IsPos (XO p)};
   XO p ->
    case y of {
     XI q -> succ_double_mask (sub_mask_carry p q);
     XO q -> double_mask (sub_mask p q);
     XH -> IsPos (pred_double p)};
   XH -> case y of {
          XH -> IsNul;
          _ -> IsNeg}}

sub_mask_carry :: Positive -> Positive -> Mask
sub_mask_carry x y =
  case x of {
   XI p ->
    case y of {
     XI q -> succ_double_mask (sub_mask_carry p q);
     XO q -> double_mask (sub_mask p q);
     XH -> IsPos (pred_double p)};
   XO p ->
    case y of {
     XI q -> double_mask (sub_mask_carry p q);
     XO q -> succ_double_mask (sub_mask_carry p q);
     XH -> double_pred_mask p};
   XH -> IsNeg}

sub :: Positive -> Positive -> Positive
sub x y =
  case sub_mask x y of {
   IsPos z -> z;
   _ -> XH}

mul :: Positive -> Positive -> Positive
mul x y =
  case x of {
   XI p -> add0 y (XO (mul p y));
   XO p -> XO (mul p y);
   XH -> y}

iter :: (a1 -> a1) -> a1 -> Positive -> a1
iter f x n =
  case n of {
   XI n' -> f (iter f (iter f x n') n');
   XO n' -> iter f (iter f x n') n';
   XH -> f x}

square :: Positive -> Positive
square p =
  case p of {
   XI p0 -> XI (XO (add0 (square p0) p0));
   XO p0 -> XO (XO (square p0));
   XH -> XH}

div2 :: Positive -> Positive
div2 p =
  case p of {
   XI p0 -> p0;
   XO p0 -> p0;
   XH -> XH}

div2_up :: Positive -> Positive
div2_up p =
  case p of {
   XI p0 -> succ p0;
   XO p0 -> p0;
   XH -> XH}

size_nat :: Positive -> Nat
size_nat p =
  case p of {
   XI p0 -> S (size_nat p0);
   XO p0 -> S (size_nat p0);
   XH -> S O}

size :: Positive -> Positive
size p =
  case p of {
   XI p0 -> succ (size p0);
   XO p0 -> succ (size p0);
   XH -> XH}

compare_cont :: Comparison -> Positive -> Positive -> Comparison
compare_cont r x y =
  case x of {
   XI p ->
    case y of {
     XI q -> compare_cont r p q;
     XO q -> compare_cont Gt p q;
     XH -> Gt};
   XO p ->
    case y of {
     XI q -> compare_cont Lt p q;
     XO q -> compare_cont r p q;
     XH -> Gt};
   XH -> case y of {
          XH -> r;
          _ -> Lt}}

compare :: Positive -> Positive -> Comparison
compare =
  compare_cont Eq

eqb :: Positive -> Positive -> Prelude.Bool
eqb p q =
  case p of {
   XI p0 -> case q of {
             XI q0 -> eqb p0 q0;
             _ -> Prelude.False};
   XO p0 -> case q of {
             XO q0 -> eqb p0 q0;
             _ -> Prelude.False};
   XH -> case q of {
          XH -> Prelude.True;
          _ -> Prelude.False}}

leb :: Positive -> Positive -> Prelude.Bool
leb x y =
  case compare x y of {
   Gt -> Prelude.False;
   _ -> Prelude.True}

sqrtrem_step :: (Positive -> Positive) -> (Positive -> Positive) -> ((,)
                Positive Mask) -> (,) Positive Mask
sqrtrem_step f g p =
  case p of {
   (,) s y ->
    case y of {
     IsPos r ->
      let {s' = XI (XO s)} in
      let {r' = g (f r)} in
      case leb s' r' of {
       Prelude.True -> (,) (XI s) (sub_mask r' s');
       Prelude.False -> (,) (XO s) (IsPos r')};
     _ -> (,) (XO s) (sub_mask (g (f XH)) (XO (XO XH)))}}

sqrtrem :: Positive -> (,) Positive Mask
sqrtrem p =
  case p of {
   XI p0 ->
    case p0 of {
     XI p1 -> sqrtrem_step (\x -> XI x) (\x -> XI x) (sqrtrem p1);
     XO p1 -> sqrtrem_step (\x -> XO x) (\x -> XI x) (sqrtrem p1);
     XH -> (,) XH (IsPos (XO XH))};
   XO p0 ->
    case p0 of {
     XI p1 -> sqrtrem_step (\x -> XI x) (\x -> XO x) (sqrtrem p1);
     XO p1 -> sqrtrem_step (\x -> XO x) (\x -> XO x) (sqrtrem p1);
     XH -> (,) XH (IsPos XH)};
   XH -> (,) XH IsNul}

sqrt :: Positive -> Positive
sqrt p =
  fst (sqrtrem p)

gcdn :: Nat -> Positive -> Positive -> Positive
gcdn n a b =
  case n of {
   O -> XH;
   S n0 ->
    case a of {
     XI a' ->
      case b of {
       XI b' ->
        case compare a' b' of {
         Eq -> a;
         Lt -> gcdn n0 (sub b' a') a;
         Gt -> gcdn n0 (sub a' b') b};
       XO b0 -> gcdn n0 a b0;
       XH -> XH};
     XO a0 ->
      case b of {
       XI _ -> gcdn n0 a0 b;
       XO b0 -> XO (gcdn n0 a0 b0);
       XH -> XH};
     XH -> XH}}

gcd :: Positive -> Positive -> Positive
gcd a b =
  gcdn (add (size_nat a) (size_nat b)) a b

ggcdn :: Nat -> Positive -> Positive -> (,) Positive ((,) Positive Positive)
ggcdn n a b =
  case n of {
   O -> (,) XH ((,) a b);
   S n0 ->
    case a of {
     XI a' ->
      case b of {
       XI b' ->
        case compare a' b' of {
         Eq -> (,) a ((,) XH XH);
         Lt ->
          case ggcdn n0 (sub b' a') a of {
           (,) g p ->
            case p of {
             (,) ba aa -> (,) g ((,) aa (add0 aa (XO ba)))}};
         Gt ->
          case ggcdn n0 (sub a' b') b of {
           (,) g p ->
            case p of {
             (,) ab bb -> (,) g ((,) (add0 bb (XO ab)) bb)}}};
       XO b0 ->
        case ggcdn n0 a b0 of {
         (,) g p -> case p of {
                     (,) aa bb -> (,) g ((,) aa (XO bb))}};
       XH -> (,) XH ((,) a XH)};
     XO a0 ->
      case b of {
       XI _ ->
        case ggcdn n0 a0 b of {
         (,) g p -> case p of {
                     (,) aa bb -> (,) g ((,) (XO aa) bb)}};
       XO b0 -> case ggcdn n0 a0 b0 of {
                 (,) g p -> (,) (XO g) p};
       XH -> (,) XH ((,) a XH)};
     XH -> (,) XH ((,) XH b)}}

ggcd :: Positive -> Positive -> (,) Positive ((,) Positive Positive)
ggcd a b =
  ggcdn (add (size_nat a) (size_nat b)) a b

nsucc_double :: N -> N
nsucc_double x =
  case x of {
   N0 -> Npos XH;
   Npos p -> Npos (XI p)}

ndouble :: N -> N
ndouble n =
  case n of {
   N0 -> N0;
   Npos p -> Npos (XO p)}

lor :: Positive -> Positive -> Positive
lor p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> XI (lor p0 q0);
     XO q0 -> XI (lor p0 q0);
     XH -> p};
   XO p0 ->
    case q of {
     XI q0 -> XI (lor p0 q0);
     XO q0 -> XO (lor p0 q0);
     XH -> XI p0};
   XH -> case q of {
          XO q0 -> XI q0;
          _ -> q}}

land :: Positive -> Positive -> N
land p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> nsucc_double (land p0 q0);
     XO q0 -> ndouble (land p0 q0);
     XH -> Npos XH};
   XO p0 ->
    case q of {
     XI q0 -> ndouble (land p0 q0);
     XO q0 -> ndouble (land p0 q0);
     XH -> N0};
   XH -> case q of {
          XO _ -> N0;
          _ -> Npos XH}}

ldiff :: Positive -> Positive -> N
ldiff p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> ndouble (ldiff p0 q0);
     XO q0 -> nsucc_double (ldiff p0 q0);
     XH -> Npos (XO p0)};
   XO p0 ->
    case q of {
     XI q0 -> ndouble (ldiff p0 q0);
     XO q0 -> ndouble (ldiff p0 q0);
     XH -> Npos p};
   XH -> case q of {
          XO _ -> Npos XH;
          _ -> N0}}

lxor :: Positive -> Positive -> N
lxor p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> ndouble (lxor p0 q0);
     XO q0 -> nsucc_double (lxor p0 q0);
     XH -> Npos (XO p0)};
   XO p0 ->
    case q of {
     XI q0 -> nsucc_double (lxor p0 q0);
     XO q0 -> ndouble (lxor p0 q0);
     XH -> Npos (XI p0)};
   XH -> case q of {
          XI q0 -> Npos (XO q0);
          XO q0 -> Npos (XI q0);
          XH -> N0}}

testbit :: Positive -> N -> Prelude.Bool
testbit p n =
  case p of {
   XI p0 ->
    case n of {
     N0 -> Prelude.True;
     Npos n0 -> testbit p0 (pred_N n0)};
   XO p0 ->
    case n of {
     N0 -> Prelude.False;
     Npos n0 -> testbit p0 (pred_N n0)};
   XH -> case n of {
          N0 -> Prelude.True;
          Npos _ -> Prelude.False}}

iter_op :: (a1 -> a1 -> a1) -> Positive -> a1 -> a1
iter_op op p a =
  case p of {
   XI p0 -> op a (iter_op op p0 (op a a));
   XO p0 -> iter_op op p0 (op a a);
   XH -> a}

to_nat :: Positive -> Nat
to_nat x =
  iter_op add x (S O)

of_succ_nat :: Nat -> Positive
of_succ_nat n =
  case n of {
   O -> XH;
   S x -> succ (of_succ_nat x)}

of_uint_acc :: Uint -> Positive -> Positive
of_uint_acc d acc =
  case d of {
   Nil -> acc;
   D0 l -> of_uint_acc l (mul (XO (XI (XO XH))) acc);
   D1 l -> of_uint_acc l (add0 XH (mul (XO (XI (XO XH))) acc));
   D2 l -> of_uint_acc l (add0 (XO XH) (mul (XO (XI (XO XH))) acc));
   D3 l -> of_uint_acc l (add0 (XI XH) (mul (XO (XI (XO XH))) acc));
   D4 l -> of_uint_acc l (add0 (XO (XO XH)) (mul (XO (XI (XO XH))) acc));
   D5 l -> of_uint_acc l (add0 (XI (XO XH)) (mul (XO (XI (XO XH))) acc));
   D6 l -> of_uint_acc l (add0 (XO (XI XH)) (mul (XO (XI (XO XH))) acc));
   D7 l -> of_uint_acc l (add0 (XI (XI XH)) (mul (XO (XI (XO XH))) acc));
   D8 l -> of_uint_acc l (add0 (XO (XO (XO XH))) (mul (XO (XI (XO XH))) acc));
   D9 l -> of_uint_acc l (add0 (XI (XO (XO XH))) (mul (XO (XI (XO XH))) acc))}

of_uint :: Uint -> N
of_uint d =
  case d of {
   Nil -> N0;
   D0 l -> of_uint l;
   D1 l -> Npos (of_uint_acc l XH);
   D2 l -> Npos (of_uint_acc l (XO XH));
   D3 l -> Npos (of_uint_acc l (XI XH));
   D4 l -> Npos (of_uint_acc l (XO (XO XH)));
   D5 l -> Npos (of_uint_acc l (XI (XO XH)));
   D6 l -> Npos (of_uint_acc l (XO (XI XH)));
   D7 l -> Npos (of_uint_acc l (XI (XI XH)));
   D8 l -> Npos (of_uint_acc l (XO (XO (XO XH))));
   D9 l -> Npos (of_uint_acc l (XI (XO (XO XH))))}

to_little_uint :: Positive -> Uint
to_little_uint p =
  case p of {
   XI p0 -> succ_double (to_little_uint p0);
   XO p0 -> double (to_little_uint p0);
   XH -> D1 Nil}

to_uint :: Positive -> Uint
to_uint p =
  rev (to_little_uint p)

succ_double0 :: N -> N
succ_double0 x =
  case x of {
   N0 -> Npos XH;
   Npos p -> Npos (XI p)}

double0 :: N -> N
double0 n =
  case n of {
   N0 -> N0;
   Npos p -> Npos (XO p)}

succ_pos :: N -> Positive
succ_pos n =
  case n of {
   N0 -> XH;
   Npos p -> succ p}

sub0 :: N -> N -> N
sub0 n m =
  case n of {
   N0 -> N0;
   Npos n' ->
    case m of {
     N0 -> n;
     Npos m' -> case sub_mask n' m' of {
                 IsPos p -> Npos p;
                 _ -> N0}}}

compare0 :: N -> N -> Comparison
compare0 n m =
  case n of {
   N0 -> case m of {
          N0 -> Eq;
          Npos _ -> Lt};
   Npos n' -> case m of {
               N0 -> Gt;
               Npos m' -> compare n' m'}}

leb0 :: N -> N -> Prelude.Bool
leb0 x y =
  case compare0 x y of {
   Gt -> Prelude.False;
   _ -> Prelude.True}

pos_div_eucl :: Positive -> N -> (,) N N
pos_div_eucl a b =
  case a of {
   XI a' ->
    case pos_div_eucl a' b of {
     (,) q r ->
      let {r' = succ_double0 r} in
      case leb0 b r' of {
       Prelude.True -> (,) (succ_double0 q) (sub0 r' b);
       Prelude.False -> (,) (double0 q) r'}};
   XO a' ->
    case pos_div_eucl a' b of {
     (,) q r ->
      let {r' = double0 r} in
      case leb0 b r' of {
       Prelude.True -> (,) (succ_double0 q) (sub0 r' b);
       Prelude.False -> (,) (double0 q) r'}};
   XH ->
    case b of {
     N0 -> (,) N0 (Npos XH);
     Npos p -> case p of {
                XH -> (,) (Npos XH) N0;
                _ -> (,) N0 (Npos XH)}}}

lor0 :: N -> N -> N
lor0 n m =
  case n of {
   N0 -> m;
   Npos p -> case m of {
              N0 -> n;
              Npos q -> Npos (lor p q)}}

land0 :: N -> N -> N
land0 n m =
  case n of {
   N0 -> N0;
   Npos p -> case m of {
              N0 -> N0;
              Npos q -> land p q}}

ldiff0 :: N -> N -> N
ldiff0 n m =
  case n of {
   N0 -> N0;
   Npos p -> case m of {
              N0 -> n;
              Npos q -> ldiff p q}}

lxor0 :: N -> N -> N
lxor0 n m =
  case n of {
   N0 -> m;
   Npos p -> case m of {
              N0 -> n;
              Npos q -> lxor p q}}

testbit0 :: N -> N -> Prelude.Bool
testbit0 a n =
  case a of {
   N0 -> Prelude.False;
   Npos p -> testbit p n}

type T = Z

zero :: Z
zero =
  Z0

one :: Z
one =
  Zpos XH

two :: Z
two =
  Zpos (XO XH)

double1 :: Z -> Z
double1 x =
  case x of {
   Z0 -> Z0;
   Zpos p -> Zpos (XO p);
   Zneg p -> Zneg (XO p)}

succ_double1 :: Z -> Z
succ_double1 x =
  case x of {
   Z0 -> Zpos XH;
   Zpos p -> Zpos (XI p);
   Zneg p -> Zneg (pred_double p)}

pred_double0 :: Z -> Z
pred_double0 x =
  case x of {
   Z0 -> Zneg XH;
   Zpos p -> Zpos (pred_double p);
   Zneg p -> Zneg (XI p)}

pos_sub :: Positive -> Positive -> Z
pos_sub x y =
  case x of {
   XI p ->
    case y of {
     XI q -> double1 (pos_sub p q);
     XO q -> succ_double1 (pos_sub p q);
     XH -> Zpos (XO p)};
   XO p ->
    case y of {
     XI q -> pred_double0 (pos_sub p q);
     XO q -> double1 (pos_sub p q);
     XH -> Zpos (pred_double p)};
   XH ->
    case y of {
     XI q -> Zneg (XO q);
     XO q -> Zneg (pred_double q);
     XH -> Z0}}

add1 :: Z -> Z -> Z
add1 x y =
  case x of {
   Z0 -> y;
   Zpos x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> Zpos (add0 x' y');
     Zneg y' -> pos_sub x' y'};
   Zneg x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> pos_sub y' x';
     Zneg y' -> Zneg (add0 x' y')}}

opp :: Z -> Z
opp x =
  case x of {
   Z0 -> Z0;
   Zpos x0 -> Zneg x0;
   Zneg x0 -> Zpos x0}

succ0 :: Z -> Z
succ0 x =
  add1 x (Zpos XH)

pred :: Z -> Z
pred x =
  add1 x (Zneg XH)

sub1 :: Z -> Z -> Z
sub1 m n =
  add1 m (opp n)

mul0 :: Z -> Z -> Z
mul0 x y =
  case x of {
   Z0 -> Z0;
   Zpos x' ->
    case y of {
     Z0 -> Z0;
     Zpos y' -> Zpos (mul x' y');
     Zneg y' -> Zneg (mul x' y')};
   Zneg x' ->
    case y of {
     Z0 -> Z0;
     Zpos y' -> Zneg (mul x' y');
     Zneg y' -> Zpos (mul x' y')}}

pow_pos :: Z -> Positive -> Z
pow_pos z =
  iter (mul0 z) (Zpos XH)

pow :: Z -> Z -> Z
pow x y =
  case y of {
   Z0 -> Zpos XH;
   Zpos p -> pow_pos x p;
   Zneg _ -> Z0}

square0 :: Z -> Z
square0 x =
  case x of {
   Z0 -> Z0;
   Zpos p -> Zpos (square p);
   Zneg p -> Zpos (square p)}

compare1 :: Z -> Z -> Comparison
compare1 x y =
  case x of {
   Z0 -> case y of {
          Z0 -> Eq;
          Zpos _ -> Lt;
          Zneg _ -> Gt};
   Zpos x' -> case y of {
               Zpos y' -> compare x' y';
               _ -> Gt};
   Zneg x' -> case y of {
               Zneg y' -> compOpp (compare x' y');
               _ -> Lt}}

sgn :: Z -> Z
sgn z =
  case z of {
   Z0 -> Z0;
   Zpos _ -> Zpos XH;
   Zneg _ -> Zneg XH}

leb1 :: Z -> Z -> Prelude.Bool
leb1 x y =
  case compare1 x y of {
   Gt -> Prelude.False;
   _ -> Prelude.True}

ltb :: Z -> Z -> Prelude.Bool
ltb x y =
  case compare1 x y of {
   Lt -> Prelude.True;
   _ -> Prelude.False}

geb :: Z -> Z -> Prelude.Bool
geb x y =
  case compare1 x y of {
   Lt -> Prelude.False;
   _ -> Prelude.True}

gtb :: Z -> Z -> Prelude.Bool
gtb x y =
  case compare1 x y of {
   Gt -> Prelude.True;
   _ -> Prelude.False}

eqb0 :: Z -> Z -> Prelude.Bool
eqb0 x y =
  case x of {
   Z0 -> case y of {
          Z0 -> Prelude.True;
          _ -> Prelude.False};
   Zpos p -> case y of {
              Zpos q -> eqb p q;
              _ -> Prelude.False};
   Zneg p -> case y of {
              Zneg q -> eqb p q;
              _ -> Prelude.False}}

max :: Z -> Z -> Z
max n m =
  case compare1 n m of {
   Lt -> m;
   _ -> n}

min :: Z -> Z -> Z
min n m =
  case compare1 n m of {
   Gt -> m;
   _ -> n}

abs :: Z -> Z
abs z =
  case z of {
   Zneg p -> Zpos p;
   x -> x}

abs_nat :: Z -> Nat
abs_nat z =
  case z of {
   Z0 -> O;
   Zpos p -> to_nat p;
   Zneg p -> to_nat p}

abs_N :: Z -> N
abs_N z =
  case z of {
   Z0 -> N0;
   Zpos p -> Npos p;
   Zneg p -> Npos p}

to_nat0 :: Z -> Nat
to_nat0 z =
  case z of {
   Zpos p -> to_nat p;
   _ -> O}

to_N :: Z -> N
to_N z =
  case z of {
   Zpos p -> Npos p;
   _ -> N0}

of_nat :: Nat -> Z
of_nat n =
  case n of {
   O -> Z0;
   S n0 -> Zpos (of_succ_nat n0)}

of_N :: N -> Z
of_N n =
  case n of {
   N0 -> Z0;
   Npos p -> Zpos p}

to_pos :: Z -> Positive
to_pos z =
  case z of {
   Zpos p -> p;
   _ -> XH}

of_uint0 :: Uint -> Z
of_uint0 d =
  of_N (of_uint d)

of_int :: Int -> Z
of_int d =
  case d of {
   Pos d0 -> of_uint0 d0;
   Neg d0 -> opp (of_uint0 d0)}

to_int :: Z -> Int
to_int n =
  case n of {
   Z0 -> Pos (D0 Nil);
   Zpos p -> Pos (to_uint p);
   Zneg p -> Neg (to_uint p)}

iter0 :: Z -> (a1 -> a1) -> a1 -> a1
iter0 n f x =
  case n of {
   Zpos p -> iter f x p;
   _ -> x}

pos_div_eucl0 :: Positive -> Z -> (,) Z Z
pos_div_eucl0 a b =
  case a of {
   XI a' ->
    case pos_div_eucl0 a' b of {
     (,) q r ->
      let {r' = add1 (mul0 (Zpos (XO XH)) r) (Zpos XH)} in
      case ltb r' b of {
       Prelude.True -> (,) (mul0 (Zpos (XO XH)) q) r';
       Prelude.False -> (,) (add1 (mul0 (Zpos (XO XH)) q) (Zpos XH))
        (sub1 r' b)}};
   XO a' ->
    case pos_div_eucl0 a' b of {
     (,) q r ->
      let {r' = mul0 (Zpos (XO XH)) r} in
      case ltb r' b of {
       Prelude.True -> (,) (mul0 (Zpos (XO XH)) q) r';
       Prelude.False -> (,) (add1 (mul0 (Zpos (XO XH)) q) (Zpos XH))
        (sub1 r' b)}};
   XH ->
    case leb1 (Zpos (XO XH)) b of {
     Prelude.True -> (,) Z0 (Zpos XH);
     Prelude.False -> (,) (Zpos XH) Z0}}

div_eucl :: Z -> Z -> (,) Z Z
div_eucl a b =
  case a of {
   Z0 -> (,) Z0 Z0;
   Zpos a' ->
    case b of {
     Z0 -> (,) Z0 Z0;
     Zpos _ -> pos_div_eucl0 a' b;
     Zneg b' ->
      case pos_div_eucl0 a' (Zpos b') of {
       (,) q r ->
        case r of {
         Z0 -> (,) (opp q) Z0;
         _ -> (,) (opp (add1 q (Zpos XH))) (add1 b r)}}};
   Zneg a' ->
    case b of {
     Z0 -> (,) Z0 Z0;
     Zpos _ ->
      case pos_div_eucl0 a' b of {
       (,) q r ->
        case r of {
         Z0 -> (,) (opp q) Z0;
         _ -> (,) (opp (add1 q (Zpos XH))) (sub1 b r)}};
     Zneg b' ->
      case pos_div_eucl0 a' (Zpos b') of {
       (,) q r -> (,) q (opp r)}}}

div :: Z -> Z -> Z
div a b =
  case div_eucl a b of {
   (,) q _ -> q}

modulo :: Z -> Z -> Z
modulo a b =
  case div_eucl a b of {
   (,) _ r -> r}

quotrem :: Z -> Z -> (,) Z Z
quotrem a b =
  case a of {
   Z0 -> (,) Z0 Z0;
   Zpos a0 ->
    case b of {
     Z0 -> (,) Z0 a;
     Zpos b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       (,) q r -> (,) (of_N q) (of_N r)};
     Zneg b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       (,) q r -> (,) (opp (of_N q)) (of_N r)}};
   Zneg a0 ->
    case b of {
     Z0 -> (,) Z0 a;
     Zpos b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       (,) q r -> (,) (opp (of_N q)) (opp (of_N r))};
     Zneg b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       (,) q r -> (,) (of_N q) (opp (of_N r))}}}

quot :: Z -> Z -> Z
quot a b =
  fst (quotrem a b)

rem :: Z -> Z -> Z
rem a b =
  snd (quotrem a b)

even :: Z -> Prelude.Bool
even z =
  case z of {
   Z0 -> Prelude.True;
   Zpos p -> case p of {
              XO _ -> Prelude.True;
              _ -> Prelude.False};
   Zneg p -> case p of {
              XO _ -> Prelude.True;
              _ -> Prelude.False}}

odd :: Z -> Prelude.Bool
odd z =
  case z of {
   Z0 -> Prelude.False;
   Zpos p -> case p of {
              XO _ -> Prelude.False;
              _ -> Prelude.True};
   Zneg p -> case p of {
              XO _ -> Prelude.False;
              _ -> Prelude.True}}

div0 :: Z -> Z
div0 z =
  case z of {
   Z0 -> Z0;
   Zpos p -> case p of {
              XH -> Z0;
              _ -> Zpos (div2 p)};
   Zneg p -> Zneg (div2_up p)}

quot2 :: Z -> Z
quot2 z =
  case z of {
   Z0 -> Z0;
   Zpos p -> case p of {
              XH -> Z0;
              _ -> Zpos (div2 p)};
   Zneg p -> case p of {
              XH -> Z0;
              _ -> Zneg (div2 p)}}

log2 :: Z -> Z
log2 z =
  case z of {
   Zpos p0 ->
    case p0 of {
     XI p -> Zpos (size p);
     XO p -> Zpos (size p);
     XH -> Z0};
   _ -> Z0}

sqrtrem0 :: Z -> (,) Z Z
sqrtrem0 n =
  case n of {
   Zpos p ->
    case sqrtrem p of {
     (,) s m ->
      case m of {
       IsPos r -> (,) (Zpos s) (Zpos r);
       _ -> (,) (Zpos s) Z0}};
   _ -> (,) Z0 Z0}

sqrt0 :: Z -> Z
sqrt0 n =
  case n of {
   Zpos p -> Zpos (sqrt p);
   _ -> Z0}

gcd0 :: Z -> Z -> Z
gcd0 a b =
  case a of {
   Z0 -> abs b;
   Zpos a0 ->
    case b of {
     Z0 -> abs a;
     Zpos b0 -> Zpos (gcd a0 b0);
     Zneg b0 -> Zpos (gcd a0 b0)};
   Zneg a0 ->
    case b of {
     Z0 -> abs a;
     Zpos b0 -> Zpos (gcd a0 b0);
     Zneg b0 -> Zpos (gcd a0 b0)}}

ggcd0 :: Z -> Z -> (,) Z ((,) Z Z)
ggcd0 a b =
  case a of {
   Z0 -> (,) (abs b) ((,) Z0 (sgn b));
   Zpos a0 ->
    case b of {
     Z0 -> (,) (abs a) ((,) (sgn a) Z0);
     Zpos b0 ->
      case ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) (Zpos g) ((,) (Zpos aa) (Zpos bb))}};
     Zneg b0 ->
      case ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) (Zpos g) ((,) (Zpos aa) (Zneg bb))}}};
   Zneg a0 ->
    case b of {
     Z0 -> (,) (abs a) ((,) (sgn a) Z0);
     Zpos b0 ->
      case ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) (Zpos g) ((,) (Zneg aa) (Zpos bb))}};
     Zneg b0 ->
      case ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) (Zpos g) ((,) (Zneg aa) (Zneg bb))}}}}

testbit1 :: Z -> Z -> Prelude.Bool
testbit1 a n =
  case n of {
   Z0 -> odd a;
   Zpos p ->
    case a of {
     Z0 -> Prelude.False;
     Zpos a0 -> testbit a0 (Npos p);
     Zneg a0 -> Prelude.not (testbit0 (pred_N a0) (Npos p))};
   Zneg _ -> Prelude.False}

shiftl :: Z -> Z -> Z
shiftl a n =
  case n of {
   Z0 -> a;
   Zpos p -> iter (mul0 (Zpos (XO XH))) a p;
   Zneg p -> iter div0 a p}

shiftr :: Z -> Z -> Z
shiftr a n =
  shiftl a (opp n)

lor1 :: Z -> Z -> Z
lor1 a b =
  case a of {
   Z0 -> b;
   Zpos a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zpos (lor a0 b0);
     Zneg b0 -> Zneg (succ_pos (ldiff0 (pred_N b0) (Npos a0)))};
   Zneg a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zneg (succ_pos (ldiff0 (pred_N a0) (Npos b0)));
     Zneg b0 -> Zneg (succ_pos (land0 (pred_N a0) (pred_N b0)))}}

land1 :: Z -> Z -> Z
land1 a b =
  case a of {
   Z0 -> Z0;
   Zpos a0 ->
    case b of {
     Z0 -> Z0;
     Zpos b0 -> of_N (land a0 b0);
     Zneg b0 -> of_N (ldiff0 (Npos a0) (pred_N b0))};
   Zneg a0 ->
    case b of {
     Z0 -> Z0;
     Zpos b0 -> of_N (ldiff0 (Npos b0) (pred_N a0));
     Zneg b0 -> Zneg (succ_pos (lor0 (pred_N a0) (pred_N b0)))}}

ldiff1 :: Z -> Z -> Z
ldiff1 a b =
  case a of {
   Z0 -> Z0;
   Zpos a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> of_N (ldiff a0 b0);
     Zneg b0 -> of_N (land0 (Npos a0) (pred_N b0))};
   Zneg a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zneg (succ_pos (lor0 (pred_N a0) (Npos b0)));
     Zneg b0 -> of_N (ldiff0 (pred_N b0) (pred_N a0))}}

lxor1 :: Z -> Z -> Z
lxor1 a b =
  case a of {
   Z0 -> b;
   Zpos a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> of_N (lxor a0 b0);
     Zneg b0 -> Zneg (succ_pos (lxor0 (Npos a0) (pred_N b0)))};
   Zneg a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zneg (succ_pos (lxor0 (pred_N a0) (Npos b0)));
     Zneg b0 -> of_N (lxor0 (pred_N a0) (pred_N b0))}}

myadd :: Z -> Z -> Z
myadd =
  add1

