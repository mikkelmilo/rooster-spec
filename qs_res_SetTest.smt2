(declare-datatype
  pair :tuple :source |(,)|
  (par (a b)
    ((pair2 :tuple :source |(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatype
  list :source |[]|
  (par (a)
    ((nil :source |[]|) (cons :source |:| (head a) (tail (list a))))))
(define-fun-rec
  set_mem :keep :source set_mem
  (par (a1) (((x (=> a1 (=> a1 Bool))) (y a1) (z (list a1))) Bool))
  (match z
    ((nil false)
     ((cons a2 x1) (ite (@ (@ x y) a2) true (set_mem x y x1))))))
(define-fun-rec
  set_inter :keep :source set_inter
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y (list a1)) (z (list a1))) (list a1)))
  (match y
    ((nil (_ nil a1))
     ((cons a2 x1)
      (ite
        (set_mem x a2 z) (cons a2 (set_inter x x1 z))
        (set_inter x x1 z))))))
(define-fun-rec
  set_add :keep :source set_add
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y a1) (z (list a1))) (list a1)))
  (match z
    ((nil (cons y (_ nil a1)))
     ((cons a2 x1)
      (ite (@ (@ x y) a2) (cons a2 x1) (cons a2 (set_add x y x1)))))))
(define-fun-rec
  set_diff :keep :source set_diff
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y (list a1)) (z (list a1))) (list a1)))
  (match y
    ((nil (_ nil a1))
     ((cons a2 x1)
      (ite
        (set_mem x a2 z) (set_diff x x1 z)
        (set_add x a2 (set_diff x x1 z)))))))
(define-fun-rec
  set_union :keep :source set_union
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y (list a1)) (z (list a1))) (list a1)))
  (match z
    ((nil y)
     ((cons a2 y1) (set_add x a2 (set_union x y y1))))))
(define-fun-rec
  map :keep :source map
  (par (a1 a2) (((x (=> a1 a2)) (y (list a1))) (list a2)))
  (match y
    ((nil (_ nil a2))
     ((cons b t) (cons (@ x b) (map x t))))))
(define-fun-rec
  fold_right :keep :source fold_right
  (par (a2 a1) (((x (=> a2 (=> a1 a1))) (y a1) (z (list a2))) a1))
  (match z
    ((nil y)
     ((cons b t) (@ (@ x b) (fold_right x y t))))))
(define-fun
  set_fold_right :keep :source set_fold_right
  (par (a1 a2) (((x (=> a1 (=> a2 a2))) (y (list a1)) (z a2)) a2))
  (fold_right x z y))
(define-fun
  set_map :keep :source set_map
  (par (a2 a1)
    (((x (=> a2 (=> a2 Bool))) (y (=> a1 a2)) (z (list a1)))
     (list a2)))
  (set_fold_right
    (lambda ((a a1)) (lambda ((x2 (list a2))) (set_add x (@ y a) x2)))
    z (_ nil a2)))
(define-fun-rec
  app :keep :source app
  (par (a1) (((x (list a1)) (y (list a1))) (list a1)))
  (match x
    ((nil y)
     ((cons b l1) (cons b (app l1 y))))))
(define-fun-rec
  list_prod :keep :source list_prod
  (par (a1 a2) (((x (list a1)) (y (list a2))) (list (pair a1 a2))))
  (match x
    ((nil (_ nil (pair a1 a2)))
     ((cons z t)
      (app (map (lambda ((y2 a2)) (pair2 z y2)) y) (list_prod t y))))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> x y))) (= (map z (_ nil x)) (_ nil y)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (set_diff y (_ nil x) z) (_ nil x)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (set_inter y z (_ nil x)) (_ nil x)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (set_inter y (_ nil x) z) (_ nil x)))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> y (=> y Bool))) (x2 (=> x y)))
      (= (set_map z x2 (_ nil x)) (_ nil y)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x))
      (not (set_mem y z (_ nil x))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (set_union y z (_ nil x)) z))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (set_union y (_ nil x) z) (set_diff y z (_ nil x))))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> y x)) (x2 y))
      (= (map z (cons x2 (_ nil y))) (cons (@ z x2) (_ nil x))))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> x (=> x Bool))) (x2 (=> y x)) (x3 (list y)))
      (= (set_diff z (map x2 x3) (_ nil x)) (set_map z x2 x3)))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> x (=> x Bool))) (x2 (=> y x)) (x3 y))
      (= (set_map z x2 (cons x3 (_ nil y)))
        (cons (@ x2 x3) (_ nil x))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x))
      (= (set_diff y (cons z (_ nil x)) (_ nil x)) (cons z (_ nil x))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (=> x (=> x Bool))) (x2 (list x)) (x3 (list x)))
      (=> (set_mem z y x3)
        (= (set_diff z (cons y x2) x3) (set_diff z x2 x3))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (=> x (=> x Bool))) (x2 (list x)) (x3 (list x)))
      (=> (set_mem z y x3)
        (= (set_inter z (cons y x2) x3) (cons y (set_inter z x2 x3)))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 x) (x3 (list x)))
      (=> (set_mem y z x3) (set_mem y z (cons x2 x3))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (=> x (=> x Bool))) (x2 (list x)) (x3 (list x)))
      (=> (set_mem z y x2)
        (= (set_union z x2 (cons y x3)) (set_union z x2 x3))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (=> x (=> x Bool))) (x3 x) (x4 x))
      (=> (set_mem x2 y z)
        (= (set_mem x2 x3 (cons x4 (_ nil x))) (@ (@ x2 x3) x4))))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> y x)) (x2 y) (x3 (list y)))
      (= (cons (@ z x2) (map z x3)) (map z (cons x2 x3))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x))
       (x3 (list x)))
      (= (set_diff z x2 (set_union y x3 x3)) (set_diff z x2 x3)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z x) (x2 (=> x (=> x Bool)))
       (x3 (=> x (=> x Bool))) (x4 (list x)) (x5 (list x)))
      (=> (set_mem y z x5)
        (= (set_diff x3 (set_diff x2 x4 x5) x4)
          (set_diff x2 (set_diff x3 x4 x4) x5))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z x) (x2 (=> x (=> x Bool))) (x3 (list x))
       (x4 (list x)))
      (=> (set_mem y z x4)
        (= (set_diff x2 (set_diff x2 x4 x4) x3)
          (set_diff x2 (set_diff x2 x4 x3) x4))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 (list x)))
      (= (set_diff y (set_inter y z x2) x2) (_ nil x)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x))
       (x3 (list x)))
      (= (set_inter z x2 (set_union y x3 x3)) (set_inter z x2 x3)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x))
       (x3 (list x)))
      (= (set_inter z (set_diff y x3 x3) x2)
        (set_diff y (set_inter z x3 x2) x3)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x))
       (x3 (list x)))
      (= (set_inter z (set_diff y x2 x3) x3)
        (set_diff y (set_inter z x2 x3) x3)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x))
       (x3 (list x)))
      (= (set_inter z (set_diff y x2 x3) x2)
        (set_diff y (set_inter z x2 x2) x3)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z x) (x2 (=> x (=> x Bool)))
       (x3 (=> x (=> x Bool))) (x4 (list x)) (x5 (list x)) (x6 (list x)))
      (=> (set_mem y z x5)
        (= (set_diff x3 (set_inter x2 x4 x6) x5)
          (set_inter x2 (set_diff x3 x4 x5) x6))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x))
       (x3 (list x)) (x4 (list x)))
      (= (set_inter z (set_inter y x2 x4) x3)
        (set_inter y (set_inter z x2 x3) x4)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 (list x)))
      (= (set_inter y (set_inter y z x2) x2) (set_inter y z x2)))))
(prove
  :lemma
  (par (x)
    (forall ((y (list x)) (z (=> x (=> x Bool))) (x2 (list x)))
      (= (set_inter z (set_union z x2 y) x2) (set_inter z x2 x2)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 x)
       (x3 (list x)))
      (= (set_mem z x2 (set_union y x3 x3)) (set_mem z x2 x3)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 (list x)))
      (= (set_union y x2 (set_inter y z x2)) x2))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 (list x)))
      (= (set_diff y (set_diff y z x2) z)
        (set_diff y (set_diff y z z) x2)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x)))
      (= (set_diff z (set_diff y x2 x2) x2)
        (set_diff y (set_diff z x2 x2) x2)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 (list x)))
      (= (set_diff y (set_diff y z x2) (_ nil x))
        (set_diff y (set_diff y z x2) x2)))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x)))
      (= (set_union z x2 (set_diff y x2 x2))
        (set_union y x2 (set_diff z x2 x2))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (set_union y z (set_diff y z (_ nil x)))
        (set_union y z (set_diff y z z))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y x) (z (list x)) (x2 (=> x (=> x Bool))) (x3 (=> x (=> x Bool)))
       (x4 (list x)) (x5 (list x)) (x6 (list x)))
      (=> (set_mem x3 y z)
        (= (set_inter x3 (set_diff x2 x4 x6) x5)
          (set_diff x2 (set_inter x3 x4 x5) x6))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z x) (x2 (=> x (=> x Bool))) (x3 (list x))
       (x4 (list x)))
      (=> (set_mem y z x3) (set_mem y z (set_union x2 x3 x4))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y x) (z (=> x (=> x Bool))) (x2 x) (x3 (=> x (=> x Bool)))
       (x4 (list x)) (x5 (list x)) (x6 (list x)))
      (=> (set_mem x3 y x5)
        (=> (set_mem z x2 x6)
          (= (set_diff x3 (set_diff x3 x4 x6) x5)
            (set_diff x3 (set_diff x3 x4 x5) x6)))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y x) (z (list x)) (x2 x) (x3 (=> x (=> x Bool)))
       (x4 (=> x (=> x Bool))) (x5 (list x)) (x6 (list x)))
      (=> (set_mem x3 y z)
        (=> (set_mem x4 x2 x6)
          (= (set_diff x3 (set_diff x4 x5 x6) (_ nil x))
            (set_diff x4 (set_diff x4 x5 x6) x6)))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (list x)) (x2 x) (x3 x) (x4 (list x)))
      (= (set_diff y z (cons x3 (cons x2 x4)))
        (set_diff y z (cons x2 (cons x3 x4)))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 x) (x3 (list x)))
      (= (set_diff y z (cons x2 (cons x2 x3)))
        (set_diff y z (cons x2 x3))))))
(prove
  :lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (list x)) (x2 x) (x3 x) (x4 (list x)))
      (= (set_inter y z (cons x3 (cons x2 x4)))
        (set_inter y z (cons x2 (cons x3 x4)))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 x) (x3 (list x)))
      (= (set_inter y z (cons x2 (cons x2 x3)))
        (set_inter y z (cons x2 x3))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 x) (x3 x) (x4 (list x)))
      (= (set_mem y z (cons x3 (cons x2 x4)))
        (set_mem y z (cons x2 (cons x3 x4)))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 x) (x3 (list x)))
      (= (set_mem y z (cons x2 (cons x2 x3)))
        (set_mem y z (cons x2 x3))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 (list x)) (x3 (list x)))
      (= (set_union y (cons z (cons z x2)) x3)
        (cons z (set_union y (cons z x2) x3))))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 (list x)))
      (= (cons z (set_diff y x2 (cons z (_ nil x))))
        (set_union y (cons z (_ nil x)) x2)))))
(prove
  :lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 x))
      (= (set_diff y (cons x2 (cons z (_ nil x))) (_ nil x))
        (set_union y (cons z (_ nil x)) (cons x2 (_ nil x)))))))
