module TreesTest where

import qualified Prelude

data Tree a =
   Leaf
 | Node a (Tree a) (Tree a)

mirror :: (Tree a1) -> Tree a1
mirror t =
  case t of {
   Leaf -> Leaf;
   Node x l r -> Node x (mirror r) (mirror l)}

