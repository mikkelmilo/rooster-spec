module TreesTest where

import qualified Prelude

negb :: Prelude.Bool -> Prelude.Bool
negb b =
  case b of {
   Prelude.True -> Prelude.False;
   Prelude.False -> Prelude.True}

data Nat =
   O
 | S Nat

length :: (([]) a1) -> Nat
length l =
  case l of {
   ([]) -> O;
   (:) _ l' -> S (length l')}

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

mul :: Nat -> Nat -> Nat
mul n m =
  case n of {
   O -> O;
   S p -> add m (mul p m)}

add0 :: Nat -> Nat -> Nat
add0 n m =
  case n of {
   O -> m;
   S p -> S (add0 p m)}

mul0 :: Nat -> Nat -> Nat
mul0 n m =
  case n of {
   O -> O;
   S p -> add0 m (mul0 p m)}

eqb :: Nat -> Nat -> Prelude.Bool
eqb n m =
  case n of {
   O -> case m of {
         O -> Prelude.True;
         S _ -> Prelude.False};
   S n' -> case m of {
            O -> Prelude.False;
            S m' -> eqb n' m'}}

pow :: Nat -> Nat -> Nat
pow n m =
  case m of {
   O -> S O;
   S m0 -> mul0 n (pow n m0)}

map :: (a1 -> a2) -> (([]) a1) -> ([]) a2
map f l =
  case l of {
   ([]) -> ([]);
   (:) a t -> (:) (f a) (map f t)}

find :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Maybe a1
find f l =
  case l of {
   ([]) -> Prelude.Nothing;
   (:) x tl ->
    case f x of {
     Prelude.True -> Prelude.Just x;
     Prelude.False -> find f tl}}

type Id = Nat
  -- singleton inductive, whose constructor was Id
  
beq_id :: Id -> Id -> Prelude.Bool
beq_id =
  eqb

data Form =
   Var Id
 | Ftrue
 | Ffalse
 | And Form Form
 | Or Form Form
 | Imp Form Form
 | Neg Form

type Valuation = Id -> Prelude.Bool

interp :: Valuation -> Form -> Prelude.Bool
interp v p =
  case p of {
   Var id -> v id;
   Ftrue -> Prelude.True;
   Ffalse -> Prelude.False;
   And f1 f2 -> (Prelude.&&) (interp v f1) (interp v f2);
   Or f1 f2 -> (Prelude.||) (interp v f1) (interp v f2);
   Imp f1 f2 ->
    case interp v f1 of {
     Prelude.True -> interp v f2;
     Prelude.False -> Prelude.True};
   Neg f -> negb (interp v f)}

contains_id :: Id -> (([]) Id) -> Prelude.Bool
contains_id i ids =
  case ids of {
   ([]) -> Prelude.False;
   (:) x xs ->
    case beq_id x i of {
     Prelude.True -> Prelude.True;
     Prelude.False -> contains_id i xs}}

merge_id_lists :: (([]) Id) -> (([]) Id) -> ([]) Id
merge_id_lists l1 l2 =
  case l1 of {
   ([]) -> l2;
   (:) x xs ->
    case contains_id x l2 of {
     Prelude.True -> merge_id_lists xs l2;
     Prelude.False -> merge_id_lists xs ((:) x l2)}}

form_ids_aux :: Form -> (([]) Id) -> ([]) Id
form_ids_aux p ids =
  case p of {
   Var id ->
    case contains_id id ids of {
     Prelude.True -> ids;
     Prelude.False -> (:) id ids};
   And f1 f2 -> merge_id_lists (form_ids_aux f1 ids) (form_ids_aux f2 ids);
   Or f1 f2 -> merge_id_lists (form_ids_aux f1 ids) (form_ids_aux f2 ids);
   Imp f1 f2 -> merge_id_lists (form_ids_aux f1 ids) (form_ids_aux f2 ids);
   Neg f -> form_ids_aux f ids;
   _ -> ids}

form_ids :: Form -> ([]) Id
form_ids p =
  form_ids_aux p ([])

type Assignment = ([]) ((,) Id Prelude.Bool)

empty_assignment :: Form -> Assignment
empty_assignment p =
  map (\id -> (,) id Prelude.False) (form_ids p)

next_assignment :: Assignment -> Assignment
next_assignment a =
  case a of {
   ([]) -> ([]);
   (:) p xs ->
    case p of {
     (,) id b ->
      case b of {
       Prelude.True -> (:) ((,) id Prelude.False) (next_assignment xs);
       Prelude.False -> (:) ((,) id Prelude.True) xs}}}

assignment_to_valuation :: Assignment -> Valuation
assignment_to_valuation a =
  let {f = \i x -> case x of {
                    (,) id _ -> beq_id i id}} in
  (\i ->
  case find (f i) a of {
   Prelude.Just p -> case p of {
                      (,) _ v -> v};
   Prelude.Nothing -> Prelude.False})

find_valuation_aux :: Form -> Assignment -> Nat -> Prelude.Maybe Valuation
find_valuation_aux p a fuel =
  let {v' = assignment_to_valuation a} in
  case fuel of {
   O ->
    case interp v' p of {
     Prelude.True -> Prelude.Just v';
     Prelude.False -> Prelude.Nothing};
   S n ->
    case interp v' p of {
     Prelude.True -> Prelude.Just v';
     Prelude.False -> find_valuation_aux p (next_assignment a) n}}

find_valuation :: Form -> Prelude.Maybe Valuation
find_valuation p =
  let {fuel = pow (S (S O)) (length (form_ids p))} in
  find_valuation_aux p (empty_assignment p) fuel

solver :: Form -> Prelude.Bool
solver p =
  case find_valuation p of {
   Prelude.Just _ -> Prelude.True;
   Prelude.Nothing -> Prelude.False}

nnf_aux :: Form -> Nat -> Form
nnf_aux p fuel =
  case fuel of {
   O -> p;
   S n ->
    case p of {
     And f1 f2 -> And (nnf_aux f1 n) (nnf_aux f2 n);
     Or f1 f2 -> Or (nnf_aux f1 n) (nnf_aux f2 n);
     Imp f1 f2 -> Imp (nnf_aux f1 n) (nnf_aux f2 n);
     Neg f0 ->
      case f0 of {
       Var _ -> p;
       Ftrue -> Ffalse;
       Ffalse -> Ftrue;
       And f1 f2 -> Or (nnf_aux (Neg f1) n) (nnf_aux (Neg f2) n);
       Or f1 f2 -> And (nnf_aux (Neg f1) n) (nnf_aux (Neg f2) n);
       Imp f1 f2 -> And (nnf_aux f1 n) (nnf_aux (Neg f2) n);
       Neg f -> nnf_aux f n};
     _ -> p}}

size_of_form :: Form -> Nat
size_of_form p =
  case p of {
   And f1 f2 -> add (size_of_form f1) (size_of_form f2);
   Or f1 f2 -> add (size_of_form f1) (size_of_form f2);
   Imp f1 f2 -> add (size_of_form f1) (size_of_form f2);
   Neg f -> S (size_of_form f);
   _ -> S O}

nnf :: Form -> Form
nnf p =
  nnf_aux p (mul (S (S O)) (size_of_form p))

nnf_solver :: Form -> Prelude.Bool
nnf_solver p =
  solver (nnf p)

