(declare-datatype
  Maybe :source Maybe
  (par (a)
    ((Nothing :source Nothing) (Just :source Just (proj1-Just a)))))
(define-fun
  maybeAppend :keep :source maybeAppend
  (par (a1)
    (((x (=> a1 (=> a1 a1))) (y (Maybe a1)) (z (Maybe a1)))
     (Maybe a1)))
  (match y
    ((Nothing z)
     ((Just x0)
      (match z
        ((Nothing (Just x0))
         ((Just y0) (Just (@ (@ x x0) y0)))))))))
(define-fun
  semigroup_Maybe :keep :source semigroup_Maybe
  (par (a1)
    (((x (=> a1 (=> a1 a1))) (y (Maybe a1)) (z (Maybe a1)))
     (Maybe a1)))
  (maybeAppend x y z))
(define-fun
  mappend :keep :source mappend
  (par (a1) (((x (=> a1 (=> a1 a1))) (y a1) (z a1)) a1))
  (@ (@ x y) z))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x x))) (z x) (x2 x))
      (=
        (@ (@ (lambda ((x3 x)) (lambda ((x4 x)) (mappend y x3 x4))) z) x2)
        (@ (@ y z) x2)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x x))) (z (=> x (=> x x))) (x2 (Maybe x)))
      (=
        (@ (lambda ((x3 (Maybe x))) (maybeAppend y (_ Nothing x) x3)) x2)
        (@ (lambda ((x3 (Maybe x))) (maybeAppend z (_ Nothing x) x3))
          x2)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x x))) (z (Maybe x)))
      (= (maybeAppend y z (_ Nothing x)) z))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x x))) (z (Maybe x)))
      (= (maybeAppend y (_ Nothing x) z) z))))
