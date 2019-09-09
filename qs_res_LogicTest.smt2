A.hs:17:1: error: Illegal binding of built-in syntax: ()
A.hs:17:1: error: Illegal binding of built-in syntax: ()
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module A where
import qualified Text.Show.Functions
import qualified Data.Constraint as QS
import qualified Data.Typeable as T
import qualified Prelude as P
import qualified QuickSpec as QS
import qualified Test.Feat as F
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen.Unsafe as QU
newtype () =
  Mk
    (forall any .
       (QC.Arbitrary any, F.Enumerable any, P.Ord any) => any)
get (Mk x) = x
(.) ::
  forall any .
    (QC.Arbitrary any, F.Enumerable any, P.Ord any, ?imp :: (),
     ?impfalserect :: Falserect) =>
      any
(.) = get (?imp)
instance QC.Arbitrary () where
  arbitrary =
    do x <- QU.capture
       case x of QU.Capture y -> P.return (Mk (y QC.arbitrary))
gen :: QC.Gen (QS.Dict (?imp :: ()))
gen =
  do x <- QC.arbitrary
     let ?imp = x in P.return QS.Dict
newtype Falserect =
  Mkfalserect
    (forall a1 . (QC.Arbitrary a1, F.Enumerable a1, P.Ord a1) => a1)
getfalserect (Mkfalserect xfalserect) = xfalserect
falserect ::
  forall a1 .
    (QC.Arbitrary a1, F.Enumerable a1, P.Ord a1, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a1
falserect = getfalserect (?impfalserect)
instance QC.Arbitrary Falserect where
  arbitrary =
    do xfalserect <- QU.capture
       case xfalserect of
         QU.Capture yfalserect ->
           P.return (Mkfalserect (yfalserect QC.arbitrary))
genfalserect :: QC.Gen (QS.Dict (?impfalserect :: Falserect))
genfalserect =
  do xfalserect <- QC.arbitrary
     let ?impfalserect = xfalserect in P.return QS.Dict
truerect ::
  forall a12 .
    (QC.Arbitrary a12, F.Enumerable a12, P.Ord a12, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a12 -> a12
truerect z = z
truerec ::
  forall a13 .
    (QC.Arbitrary a13, F.Enumerable a13, P.Ord a13, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a13 -> a13
truerec x2 = x2
falserec ::
  forall a14 .
    (QC.Arbitrary a14, F.Enumerable a14, P.Ord a14, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a14
falserec = falserect :: a14
eqrectr ::
  forall a15 a2 .
    (QC.Arbitrary a15, F.Enumerable a15, P.Ord a15, QC.Arbitrary a2,
     F.Enumerable a2, P.Ord a2, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a15 -> a2 -> a15 -> a2
eqrectr x3 y2 z2 = y2
eqrect ::
  forall a16 a22 .
    (QC.Arbitrary a16, F.Enumerable a16, P.Ord a16, QC.Arbitrary a22,
     F.Enumerable a22, P.Ord a22, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a16 -> a22 -> a16 -> a22
eqrect x4 y3 z3 = y3
eqrecr ::
  forall a17 a23 .
    (QC.Arbitrary a17, F.Enumerable a17, P.Ord a17, QC.Arbitrary a23,
     F.Enumerable a23, P.Ord a23, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a17 -> a23 -> a17 -> a23
eqrecr x5 y4 z4 = y4
eqrec ::
  forall a18 a24 .
    (QC.Arbitrary a18, F.Enumerable a18, P.Ord a18, QC.Arbitrary a24,
     F.Enumerable a24, P.Ord a24, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a18 -> a24 -> a18 -> a24
eqrec x6 y5 z5 = y5
andrect ::
  forall a19 .
    (QC.Arbitrary a19, F.Enumerable a19, P.Ord a19, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a19 -> a19
andrect x7 = x7
andrec ::
  forall a110 .
    (QC.Arbitrary a110, F.Enumerable a110, P.Ord a110, ?imp :: (),
     ?impfalserect :: Falserect) =>
      a110 -> a110
andrec x8 = x8
sig =
  [QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable P.Rational)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable P.Bool)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary P.Rational)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary P.Bool)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (T.Typeable P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary P.Int)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary P.Rational)),
   QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.CoArbitrary P.Bool)),
   QS.inst (\ () -> gen),
   QS.inst (\ () -> genfalserect),
   QS.withMaxTermSize (4),
   QS.withMaxTestSize (12)]
