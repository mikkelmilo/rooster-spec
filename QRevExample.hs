module QRevExample where

import qualified Prelude

data List a =
   Nil
 | Cons a (List a)

app :: (List a1) -> (List a1) -> List a1
app l m =
  case l of {
   Nil -> m;
   Cons a l1 -> Cons a (app l1 m)}

zzrev :: (List a1) -> List a1
zzrev l =
  case l of {
   Nil -> Nil;
   Cons x l' -> app (zzrev l') (Cons x Nil)}

zzrev_append :: (List a1) -> (List a1) -> List a1
zzrev_append l l' =
  case l of {
   Nil -> l';
   Cons a l0 -> zzrev_append l0 (Cons a l')}

qzzrev :: (List a1) -> List a1
qzzrev xs =
  zzrev_append xs Nil

