module LogicTest where

import qualified Prelude

__ :: any
__ = Prelude.error "Logical or arity value used"

true_rect :: a1 -> a1
true_rect f =
  f

true_rec :: a1 -> a1
true_rec =
  true_rect

false_rect :: a1
false_rect =
  Prelude.error "absurd case"

false_rec :: a1
false_rec =
  false_rect

type Not = ()

and_rect :: (() -> () -> a1) -> a1
and_rect f =
  f __ __

and_rec :: (() -> () -> a1) -> a1
and_rec =
  and_rect

eq_rect :: a1 -> a2 -> a1 -> a2
eq_rect _ f _ =
  f

eq_rec :: a1 -> a2 -> a1 -> a2
eq_rec =
  eq_rect

eq_rec_r :: a1 -> a2 -> a1 -> a2
eq_rec_r =
  eq_rec

eq_rect_r :: a1 -> a2 -> a1 -> a2
eq_rect_r =
  eq_rect

