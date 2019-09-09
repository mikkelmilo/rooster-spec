(declare-datatype
  list :source |[]|
  (par (a)
    ((nil :source |[]|) (cons :source |:| (head a) (tail (list a))))))
(define-fun
  ret :keep :source ret
  (par (a1) (((x a1)) (list a1))) (cons x (_ nil a1)))
(define-fun-rec
  app :keep :source app
  (par (a1) (((x (list a1)) (y (list a1))) (list a1)))
  (match x
    ((nil y)
     ((cons b l1) (cons b (app l1 y))))))
(define-fun-rec
  flat_map :keep :source flat_map
  (par (a1 a2) (((x (=> a1 (list a2))) (y (list a1))) (list a2)))
  (match y
    ((nil (_ nil a2))
     ((cons z t) (app (@ x z) (flat_map x t))))))
(define-fun
  bind :keep :source bind
  (par (a1 a2) (((x (list a1)) (y (=> a1 (list a2)))) (list a2)))
  (flat_map y x))
(define-fun
  kleislicomp :keep :source kleislicomp
  (par (a1 a2 a3)
    (((x (=> a1 (list a2))) (y (=> a2 (list a3))) (z a1)) (list a3)))
  (bind (@ x z) y))
(prove
  :lemma
  (par (x) (forall ((y x)) (= (cons y (_ nil x)) (ret y)))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> x (list y)))) (= (bind (_ nil x) z) (_ nil y)))))
(prove
  :lemma
  (par (x)
    (forall ((y (list x))) (= (bind y (lambda ((z x)) (ret z))) y))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> x (list y))) (x2 x))
      (=
        (@ (lambda ((x3 x)) (kleislicomp (lambda ((x4 x)) (ret x4)) z x3))
          x2)
        (@ z x2)))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> x (list y))) (x2 x))
      (=
        (@ (lambda ((x3 x)) (kleislicomp z (lambda ((x4 y)) (ret x4)) x3))
          x2)
        (@ z x2)))))
(prove
  :lemma
  (par (x y z)
    (forall ((x2 (list z)) (x3 (=> z (list y))) (x4 (=> y (list x))))
      (= (bind x2 (lambda ((x5 z)) (kleislicomp x3 x4 x5)))
        (bind (bind x2 x3) x4)))))
(prove
  :lemma
  (par (x y)
    (forall ((z y) (x2 (list y)) (x3 (=> y (list x))))
      (= (kleislicomp (lambda ((x4 (list y))) (cons z x4)) x3 x2)
        (bind (cons z x2) x3)))))
(prove
  :lemma
  (par (x y z x2)
    (forall
      ((x3 (=> x (list z))) (x4 (=> z (list x2))) (x5 (=> x2 (list y)))
       (x6 x))
      (=
        (@
          (lambda ((x7 x))
            (kleislicomp (lambda ((x8 x)) (kleislicomp x3 x4 x8)) x5 x7))
          x6)
        (@
          (lambda ((x7 x))
            (kleislicomp x3 (lambda ((x9 z)) (kleislicomp x4 x5 x9)) x7))
          x6)))))
(prove
  :lemma
  (par (x y z)
    (forall ((x2 (=> y (list z))) (x3 (=> z (list x))) (x4 y))
      (= (bind (bind (ret x4) x2) x3) (kleislicomp x2 x3 x4)))))
(prove
  :lemma
  (par (x y z x2)
    (forall
      ((x3 (=> (list z) (list x2))) (x4 (=> x2 (list y)))
       (x5 (=> y (list x))))
      (=
        (kleislicomp x3
          (lambda ((x6 x2)) (kleislicomp x4 x5 x6)) (_ nil z))
        (bind (kleislicomp x3 x4 (_ nil z)) x5)))))
(prove
  :lemma
  (par (x y z x2)
    (forall
      ((x3 (=> (list z) (list x2))) (x4 (=> x2 (list y))) (x5 z)
       (x6 (=> y (list x))))
      (=
        (kleislicomp x3 (lambda ((x7 x2)) (kleislicomp x4 x6 x7)) (ret x5))
        (bind (kleislicomp x3 x4 (ret x5)) x6)))))
