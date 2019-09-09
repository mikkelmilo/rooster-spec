module GenericGroupTest where

import qualified Prelude

data Group0 g =
   Group (g -> g -> Prelude.Bool) (g -> g -> g) (g -> g -> g)

