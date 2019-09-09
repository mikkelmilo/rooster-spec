module SetTest where

import qualified Prelude

app :: (([]) a1) -> (([]) a1) -> ([]) a1
app l m =
  case l of {
   ([]) -> m;
   (:) a l1 -> (:) a (app l1 m)}

map :: (a1 -> a2) -> (([]) a1) -> ([]) a2
map f l =
  case l of {
   ([]) -> ([]);
   (:) a t -> (:) (f a) (map f t)}

fold_right :: (a2 -> a1 -> a1) -> a1 -> (([]) a2) -> a1
fold_right f a0 l =
  case l of {
   ([]) -> a0;
   (:) b t -> f b (fold_right f a0 t)}

list_prod :: (([]) a1) -> (([]) a2) -> ([]) ((,) a1 a2)
list_prod l l' =
  case l of {
   ([]) -> ([]);
   (:) x t -> app (map (\y -> (,) x y) l') (list_prod t l')}

type Set a = ([]) a

empty_set :: Set a1
empty_set =
  ([])

set_add :: (a1 -> a1 -> Prelude.Bool) -> a1 -> (Set a1) -> Set a1
set_add aeq_dec a x =
  case x of {
   ([]) -> (:) a ([]);
   (:) a1 x1 ->
    case aeq_dec a a1 of {
     Prelude.True -> (:) a1 x1;
     Prelude.False -> (:) a1 (set_add aeq_dec a x1)}}

set_mem :: (a1 -> a1 -> Prelude.Bool) -> a1 -> (Set a1) -> Prelude.Bool
set_mem aeq_dec a x =
  case x of {
   ([]) -> Prelude.False;
   (:) a1 x1 ->
    case aeq_dec a a1 of {
     Prelude.True -> Prelude.True;
     Prelude.False -> set_mem aeq_dec a x1}}

set_inter :: (a1 -> a1 -> Prelude.Bool) -> (Set a1) -> (Set a1) -> Set a1
set_inter aeq_dec x y =
  case x of {
   ([]) -> ([]);
   (:) a1 x1 ->
    case set_mem aeq_dec a1 y of {
     Prelude.True -> (:) a1 (set_inter aeq_dec x1 y);
     Prelude.False -> set_inter aeq_dec x1 y}}

set_union :: (a1 -> a1 -> Prelude.Bool) -> (Set a1) -> (Set a1) -> Set a1
set_union aeq_dec x y =
  case y of {
   ([]) -> x;
   (:) a1 y1 -> set_add aeq_dec a1 (set_union aeq_dec x y1)}

set_diff :: (a1 -> a1 -> Prelude.Bool) -> (Set a1) -> (Set a1) -> Set a1
set_diff aeq_dec x y =
  case x of {
   ([]) -> ([]);
   (:) a1 x1 ->
    case set_mem aeq_dec a1 y of {
     Prelude.True -> set_diff aeq_dec x1 y;
     Prelude.False -> set_add aeq_dec a1 (set_diff aeq_dec x1 y)}}

set_prod :: (Set a1) -> (Set a2) -> Set ((,) a1 a2)
set_prod =
  list_prod

set_fold_right :: (a1 -> a2 -> a2) -> (Set a1) -> a2 -> a2
set_fold_right f x b =
  fold_right f b x

set_map :: (a2 -> a2 -> Prelude.Bool) -> (a1 -> a2) -> (Set a1) -> Set a2
set_map aeq_dec f x =
  set_fold_right (\a -> set_add aeq_dec (f a)) x empty_set

