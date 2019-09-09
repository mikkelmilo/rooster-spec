{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module CategoryTest where

import qualified Prelude

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
#else
-- HUGS
import qualified IOExts
#endif

#ifdef __GLASGOW_HASKELL__
type Any = GHC.Base.Any
#else
-- HUGS
type Any = ()
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

data Category =
   Build_Category (Any -> Any) (Any -> Any -> Any -> Any -> Any -> Any)

c_id :: Category -> Any -> Any
c_id category =
  case category of {
   Build_Category c_id0 _ -> c_id0}

c_comp :: Category -> Any -> Any -> Any -> Any -> Any -> Any
c_comp category =
  case category of {
   Build_Category _ c_comp0 -> c_comp0}

epi_compose :: ()
epi_compose =
  __

opposite :: Category -> Category
opposite c =
  Build_Category (c_id c) (\a b c0 f g -> c_comp c c0 b a g f)

op :: Category -> Any -> Any -> Any -> Any
op _ _ _ x0 =
  x0

unop :: Category -> Any -> Any -> Any -> Any
unop _ _ _ x0 =
  x0

