module MonadTest where

import qualified Prelude

data Option a =
   Some a
 | None

retOpt :: a1 -> Option a1
retOpt x =
  Some x

bindOpt :: (Option a1) -> (a1 -> Option a2) -> Option a2
bindOpt a f =
  case a of {
   Some x -> f x;
   None -> None}

