module ListMonadTest where

import qualified Prelude

app :: (([]) a1) -> (([]) a1) -> ([]) a1
app l m =
  case l of {
   ([]) -> m;
   (:) a l1 -> (:) a (app l1 m)}

flat_map :: (a1 -> ([]) a2) -> (([]) a1) -> ([]) a2
flat_map f l =
  case l of {
   ([]) -> ([]);
   (:) x t -> app (f x) (flat_map f t)}

ret :: a1 -> ([]) a1
ret x =
  (:) x ([])

bind :: (([]) a1) -> (a1 -> ([]) a2) -> ([]) a2
bind l f =
  flat_map f l

kleislicomp :: (a1 -> ([]) a2) -> (a2 -> ([]) a3) -> a1 -> ([]) a3
kleislicomp f g a =
  bind (f a) g

