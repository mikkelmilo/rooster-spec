module SemigroupTest where

import qualified Prelude

type Semigroup m =
  m -> m -> m
  -- singleton inductive, whose constructor was Build_Semigroup
  
mappend :: (Semigroup a1) -> a1 -> a1 -> a1
mappend semigroup =
  semigroup

maybeAppend :: (Semigroup a1) -> (Prelude.Maybe a1) -> (Prelude.Maybe 
               a1) -> Prelude.Maybe a1
maybeAppend h0 x y =
  case x of {
   Prelude.Just x0 ->
    case y of {
     Prelude.Just y0 -> Prelude.Just (mappend h0 x0 y0);
     Prelude.Nothing -> x};
   Prelude.Nothing -> y}

semigroup_Maybe :: (Semigroup a1) -> Semigroup (Prelude.Maybe a1)
semigroup_Maybe =
  maybeAppend

