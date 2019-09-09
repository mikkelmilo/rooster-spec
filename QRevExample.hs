module QRevExample where

import qualified Prelude

rev :: (([]) a1) -> ([]) a1
rev l =
  case l of {
   ([]) -> ([]);
   (:) x l' -> (Prelude.++) (rev l') ((:) x ([]))}

myqrev :: (([]) a1) -> (([]) a1) -> ([]) a1
myqrev l l' =
  case l of {
   ([]) -> l';
   (:) a l0 -> myqrev l0 ((:) a l')}

