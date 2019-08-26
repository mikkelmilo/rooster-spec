module Test where

import qualified Prelude

data List a =
   Nil
 | Cons a (List a)

app :: (List a1) -> (List a1) -> List a1
app l m =
  case l of {
   Nil -> m;
   Cons a l1 -> Cons a (app l1 m)}

rev :: (List a1) -> List a1
rev l =
  case l of {
   Nil -> Nil;
   Cons x l' -> app (rev l') (Cons x Nil)}

rev_append :: (List a1) -> (List a1) -> List a1
rev_append l l' =
  case l of {
   Nil -> l';
   Cons a l0 -> rev_append l0 (Cons a l')}

qrev :: (List a1) -> List a1
qrev xs =
  rev_append xs Nil

